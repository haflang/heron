{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE TypeFamilyDependencies #-}

{-| A simple stack implementation -}
module Heron.Core.SnoopStack
 ( -- * Generation
   newSnoopStack
 , -- * Snoop Stack Types
   SnoopWarn
 , SnoopRet
 , SnoopStack
 ) where

import           Clash.Prelude
import           Data.Maybe       (isJust, isNothing)
import           Heron.Core.Stack

-- | A warning signal saying we've reached the the end of a bottom-up sweep
-- (you're looking at the head and it has been popped, or your looking at the
-- head and it has been overwritten)
type SnoopWarn = Bool

-- | A signal saying the snooping operation has completed.
type SnoopRet = Bool

-- | A stack contains elements of type @a@ with a depth of @d@ elements and a
--   pop-before-push semantics. The second port allows sneaky random access
--   outside the normal stack semantics.
type SnoopStack dom a d
  =  Signal dom (SIn a)
  -> Signal dom (RamOp d a)
  -> ( Signal dom (SOut a d)
     , Signal dom (SOut a d)
     , Signal dom SnoopRet
     , Signal dom SnoopWarn
     )

-- | Memory primitive for snoop stack. Usually BRAM or UltraRAM.
-- | It's important that this memory has a WRITE_FIRST mode at least on the
-- second port. UltraRAM behaves this was (port B ops are scheduled after A), as
-- do BlockRAMs configured in a write-first/read-new mode.
type RamPrim dom a d
  =  Signal dom (RamOp d a)
  -- ^ RAM operation for port A
  -> Signal dom (RamOp d a)
  -- ^ RAM operation for port B
  -> (Signal dom a, Signal dom a)
  -- ^ Port read outputs

-- | Construct a stack using the given memory primitive.
--  Don't read address 0 from snoop port or it might explode.
newSnoopStack
  :: forall d a dom .
     ( KnownNat d
     , NFDataX  a
     , 1 <= d
     , HiddenClockResetEnable dom )
  => RamPrim dom a d -> SnoopStack dom a d
newSnoopStack ramPrim inps sOp =
  ( SOut <$> sp <*> top   <*> top -- SnoopStacks don't do prediction of newTop. Should probably be a different data type...
  , SOut <$> sp <*> snoop <*> snoop
  , ret
  , warn
  )
  where
    -- Unpack inputs
    (mpush, pop) = unbundle inps
    isPush = isJust <$> mpush

    -- Stack size tracking
    applyOffset sz doPop doPush =
      case (doPop, doPush) of
        (False, False) -> sz
        (False, True ) -> sz+1 -- Infer two parallel adders rather than two cascaded
        (True , False) -> sz-1
        (True , True ) -> sz
    sp  = register (0 :: RamAddr d) sp'
    sp' = applyOffset <$> sp <*> pop <*> isPush

    -- Generate RAM inputs
    ramRead  = RamRead <$> sp'
    (ramOp, ret', warn') = unbundle $ arbitrate <$> sp <*> sp' <*> mpush <*> pop <*> sOp

    -- Gather RAM outputs
    (opOut, readOut) = ramPrim ramOp ramRead
    top = mux (pure 0 .==. sp) (pure Nothing) (Just <$> readOut)
    snoop = Just <$> opOut
    ret = register False ret'
    warn = register False warn'
    -- TODO Normal stack registered top outside or ram. Should we do that too for better timing?

-- TODO We should probably remove all writing functionality from the snoop port?
arbitrate :: KnownNat d => RamAddr d -> RamAddr d -> Maybe a -> Bool -> RamOp d a -> (RamOp d a, SnoopRet, SnoopWarn)
arbitrate sp sp' mpush _pop aux =
  let aux' = if warn' then RamNoOp else aux
      op = maybe aux' (RamWrite sp') mpush -- op is either push or aux
      ret' = isNothing mpush && not (isNoOp aux') -- we've scheduled a useful snoop op
      saddr = getAddr aux
      warn' = maybe False (sp <) saddr && maybe False (sp' <) saddr -- || (Just sp == saddr && pop)
  in (op, ret', warn')

isNoOp :: RamOp n a -> Bool
isNoOp RamNoOp = True
isNoOp _       = False

getAddr :: KnownNat n => RamOp n a -> Maybe (Index n)
getAddr (RamWrite addr _) = Just addr
getAddr (RamRead  addr  ) = Just addr
getAddr _                 = Nothing
