{-# LANGUAGE RecordWildCards, LambdaCase, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-| The Heron Core's control logic. This captures the core's combinatorial logic
  for performing reductions and controlling execution phases (reduction, GC,
  stalls, etc.).

  The style is heavily inspired by [Gergő Érdi's Retrocomputing with Clash
  book](https://erdi.dev/retroclash/). Use of `barbies` is to let us use just
  /one/ record type for both combinatorial logic and collections of synchronous
  signals.
-}
module Heron.Core.Core
  ( CPUIn(..)
  , CPUOut(..)
  , Phase(..)
  , cpu
  ) where

import Clash.Prelude hiding (read)
import Clash.Annotations.BitRepresentation.Deriving
import RetroClash.Barbies
import RetroClash.CPU hiding (update)
import Control.Monad
import Control.Monad.State
import Data.Maybe (fromMaybe, isJust, fromJust)
import Control.Lens hiding (Index, assign, (:>), op, imap)
import Barbies.TH
import qualified Prelude as P

import Heron.Template
import Heron.Core.Stack
import Heron.Core.ParStack
import Heron.Core.Heap
import Heron.Core.Rom
import Heron.Core.Alu
import Heron.Core.Types

-- CPU data types

-- | CPU input record. Includes outputs from all memory components, and a
-- `begin` trigger.
declareBareB [d|
  data CPUIn = CPUIn
    { uStkIn :: SOut    (Index VStkSize, HeapAddr) UStkSize
    , aStkIn :: SOut    CaseTable                  AStkSize
    , vStkIn :: PSOut   Atom      Log2MaxPush      VStkSize
    , heapIn :: HeapOut HeapNode  MaxAps           HeapSize
    , tmplIn :: RomOut  Template
    , begin  :: Maybe   TemplAddr
    } |]
deriving instance Show    (Pure CPUIn)
deriving instance Generic (Pure CPUIn)
deriving instance NFDataX (Pure CPUIn)
deriving instance ShowX   (Pure CPUIn)

-- | CPU output record. Includes inputs for all memory components, and the
-- `_result` node.
declareBareB [d|
  data CPUOut = CPUOut
    { _uStkPush :: Maybe (Index VStkSize, HeapAddr)
    , _uStkPop  :: Bool
    , _aStkPush :: Maybe CaseTable
    , _aStkPop  :: Bool
    , _vStkOut  :: PSIn   Atom      Log2MaxPush
    , _heapOut  :: HeapIn HeapNode  MaxAps         HeapSize
    , _tmplOut  :: RomIn  RomSize
    , _result   :: Maybe (Vec MaxPush (Maybe Atom))
    } |]
makeLenses ''CPUOut
deriving instance Show    (Pure CPUOut)
deriving instance Generic (Pure CPUOut)
deriving instance NFDataX (Pure CPUOut)
deriving instance ShowX   (Pure CPUOut)

-- | Current CPU execution phase
data Phase
  = Init      -- ^ Initialising
  | Reduce    -- ^ Performing reductions
  | Halt      -- ^ Finished reducing
  | GC        -- ^ Performing garbage collection (not yet implemented!)
  deriving (Show, Generic, NFDataX, Enum, Bounded, Eq)
deriveAnnotation (simpleDerivator OneHot OverlapL) [t| Phase |]
deriveBitPack [t| Phase |]

data CPUState = CPUState
  { _phase :: Phase
  , _top'  :: Atom
  , _alt'  :: Maybe CaseTable
  , _regs  :: Vec MaxRegs Atom
  }
  deriving (Generic, NFDataX)
makeLenses ''CPUState

initCPUState :: CPUState
initCPUState = CPUState
  { _phase = Init
  , _top'  = PrimInt 0
  , _alt'  = Nothing
  , _regs  = repeat (PrimInt 0)
  }

defaultOutput :: CPUState -> Pure CPUOut
defaultOutput CPUState{..} = CPUOut
  { _uStkPush = Nothing
  , _uStkPop  = False
  , _aStkPush = Nothing
  , _aStkPop  = False
  , _vStkOut = Nothing
  , _heapOut = heapOp _top'      -- Fetch when top is pointer (INV2)
  , _tmplOut = romOp _top' _alt' -- Fetch when top is FUN/CON (INV1)
  , _result  = Nothing
  }
  where
    heapOp = maybe heapNoOp (\a -> repeat RamNoOp ++ singleton (RamRead a)) . heapAddr
    romOp  (Fun _ a _  ) _                              = a
    romOp  (Con _ tag  ) (Just alt)                     = alt + bitCoerce (resize tag)
    romOp  _             _                              = 0

-- | Synchronous control logic, packaged as a Mealy machine
cpu :: (HiddenClockResetEnable dom)
    => Signals dom CPUIn -> Signals dom CPUOut
cpu = mealyCPU initCPUState defaultOutput step

cpuMachine :: Pure CPUIn -> State CPUState (Pure CPUOut)
cpuMachine = runCPU defaultOutput . step

type CPU = CPUM CPUState CPUOut

-- Dispatch for CPU phases

step :: Pure CPUIn -> CPU ()
step ins@CPUIn{..} =
  use phase >>= \case
    Halt   ->
      result .:= Just (takeI $ read vStkIn) >>
      phase   .= Init
    Init   ->
      case begin of
        Nothing       -> pure ()
        Just initAddr ->
          phase .= Reduce >>
          updateV 0 (Just (Fun 0 initAddr True) :> repeat Nothing)
    Reduce ->
      if halt    then phase .= Halt else
      if needsGC then phase .= GC   else
      top' .= top  vStkIn >>
      alt' .= _newTop aStkIn >>
      reduce (top vStkIn) ins
    GC -> error "Core.Core.step: GC not implemented"
  where
    heapFull = size heapIn > maxBound - 199
    needsGC  = heapFull && canGC (top vStkIn)
    halt     = size vStkIn < 1 && isInt (top vStkIn)

-- Dispatch for reduction rules

reduce :: Atom -> Pure CPUIn -> CPU ()
reduce t ins
  | needsUnwind t      = unwind ins
  | needsUpdate t  ins = update ins
  | needsUnfold t  ins = unfold ins
reduce (PrimInt _) ins = prim ins
reduce t _   =
  error $ "Sim.Core.reduce: Unexpected top atom." P.++ show t

-- Reduction rules

-- Unwind a heap application onto the stack
unwind :: Pure CPUIn -> CPU ()
unwind CPUIn{..} =
  do t <- use top'

     let haddr = fromJust $ heapAddr t
     let shared = isShared t

     let node = last $ read heapIn
     let unode = unpackNode shared node

     -- Possibly register update address
     when (nUpdatable unode)
          (pushU (size vStkIn, haddr))

     -- Possibly register case table
     when (isJust $ nCaseTable unode)
          (pushA $ fromJust $ nCaseTable unode)

     -- Push atoms onto stack
     let offset = (unpack . resize . pack $ nArity unode) - 1
     updateV offset
             (map (fmap (dashIf shared)) (nAtoms unode))

-- Perform primitive operations
prim :: Pure CPUIn -> CPU ()
prim CPUIn{..} =
  case primOpPat (read vStkIn) of
    -- Args already evaled; perform op
    BothInt x (_, swp, op) y ->
      do let ans = alu $ AluIn op swp x y
         updateV (-2) (Just ans :> repeat Nothing)

    -- Second arg unevaluated; swap
    SndThunk x (ar, swp, op) y ->
      updateV 0 (Just y :> Just (PrimOp ar (not swp) op) :> Just (PrimInt x) :> repeat Nothing)

    -- Special case for SEQ
    Seq x f ->
      updateV (-1) (Just f :> Just (PrimInt x) :> repeat Nothing)

    -- Unexpected prim op pattern
    NotPrim -> error $ "Core.Core.prim: Malformed args on value stack: " <> show (read vStkIn)

-- Update a heap node with its normal form
update :: Pure CPUIn -> CPU ()
update CPUIn{..} = popU >> case read uStkIn of
  Nothing ->
    error $ "Core.Core.update: Read from empty update stack"
  Just (_,uAddr) -> do

    --Dash the NF application on the stack, marking it as possibly shared
    let stkAs   = read vStkIn
    let nfArity = atomArity (fromJust . head $ read vStkIn)
    let stkAs'  = imap (\i a -> if (i < resize nfArity)
                                  then fmap dash a
                                  else Nothing) stkAs
    updateV 0 stkAs'

    -- Write the normal form to heap
    case nfArity <= nodeLen of

      -- Small normal form: Fits in one heap node
      True ->
        let n = App True (resize nfArity) False (takeI stkAs')
        in heapOut .:= heapWriteSingle (uAddr, n)

      -- Large normal form: Split into two heap nodes
      -- e.g. NF [a,b,c,d,e,f] -> x |-> [a,b,c,d]; y |-> [@x, e, f]
      False ->
        let n1 = App True (resize nodeLen) False (takeI stkAs')
            n1Addr = size heapIn
            n2Args = takeI $ dropI @NodeLen stkAs' ++
              repeat @NodeLen Nothing
            n2 = App True (resize $ nfArity + 1 - nodeLen) False
                     (Just (Ptr True n1Addr) :> n2Args)
        in heapOut .:= heapWriteN ((uAddr, n2) :> (n1Addr, n1) :> Nil)
  where
    nodeLen = snatToNum (SNat @NodeLen)

-- Instantiate a template
unfold :: Pure CPUIn -> CPU ()
unfold CPUIn{..} =
  do -- If we came from a case expression, pop from alternative stack
     let t = fromJust . head $ read vStkIn
     when (isCon t)
          popA

     let args = takeI $ read vStkIn

     -- Instantiate all template atoms
     -- Resolves ARGs, REGs, and PTRs
     primRegs <- use regs
     let hp    = size heapIn
     let Template pushOffset spineAp heapAps =
           mapTemplate (inst hp args primRegs)
                       (tmplIn)

     -- Push spine ap to stacks
     let uSpineAp = unpackNode False spineAp
     updateV pushOffset (nAtoms uSpineAp)
     maybe (return ()) pushA (nCaseTable uSpineAp)

     -- Instantiate apps on heap and PRS regs
     let (_, heapCtrl, regs') = foldl
           instNode
           (hp, repeat @MaxAps RamNoOp, primRegs)
           heapAps
     regs     .= regs'
     arbitrateHeap heapCtrl heapAps hp
  where
    -- Incrementally build controls for instantiating heap nodes
    -- PRS makes this tricky since we might not allocate anything to the heap
    instNode (addr, ctrls, rs) (Just (Prim reg _ _ as)) =
      case primOpPat as of
        -- If arguments are ints, reduce op now; no heap allocation
        BothInt x (_, _, op) y ->
          ( addr
          , ctrls
          , replace reg (alu $ AluIn op False x y) rs
          )
        -- Otherwise, dump onto heap
        _ ->
          let app = App False 3 False (as ++ repeat Nothing) in
          ( addr+1
          , RamWrite addr app +>> ctrls
          , replace reg (Ptr False addr) rs
          )
    -- Non-PRS nodes are just allocated
    instNode (addr, ctrls, rs) (Just n) =
      ( addr+1
      , RamWrite addr n +>> ctrls
      , rs
      )
    -- Ignore empties
    instNode acc Nothing = acc

    -- If we also need to prefetch a heap node, try to avoid a stall
    arbitrateHeap ctrls aps hp = do
      -- Do we need to attempt prefetching (next cycle is `unwind`)?
      t' <- use top'
      case heapAddr t' of
        -- Nope, we're OK
        Nothing   -> heapOut .:= ctrls
        -- Yes, but is it problematic?
        Just addr -> heapOut .:= (init ctrls ++ singleton (RamRead addr))

-- Helper functions

needsUnwind :: Atom -> Bool
needsUnwind (Ptr _ _) = True
needsUnwind _         = False

needsUpdate :: Atom -> Pure CPUIn -> Bool
needsUpdate t CPUIn{..}
  = maybe False go (read uStkIn)
  where
    go (uLen, _)
      = resize (atomArity t) > satSub SatBound (size vStkIn) uLen

needsUnfold :: Atom -> Pure CPUIn -> Bool
needsUnfold t CPUIn{..} = go t (_top aStkIn)
  where
    go (Fun _ _ _) _ = True
    go (Con _ _)   _ = True
    go _ _ = False

-- Instantiate an atom relative to current heap pointer, top spine values, and
-- register contents
inst :: HeapAddr -> Vec CMaxPush (Maybe Atom) -> Vec MaxRegs Atom ->
        Atom -> Atom
inst _ spine _ (Arg shared indx)
  = let arg = spine !! (1 + resize indx :: Index CMaxPush)
        arg' = fromMaybe (error $ "Core.Core.inst: Empty arg from spine")
                          arg
    in dashIf shared arg'
inst heapSize _ _ (Ptr sh addr)
  = Ptr sh $ heapSize `rawAdd` addr
inst _ _ regsv (Reg shared indx)
  = let arg = regsv !! indx
    in dashIf shared arg
inst _ _ _ a = a

pushA :: CaseTable -> CPU ()
pushA tab = aStkPush .:= Just tab

popA :: CPU ()
popA = aStkPop .:= True

pushU :: (Index VStkSize, HeapAddr) -> CPU ()
pushU u = uStkPush .:= Just u

popU :: CPU ()
popU = uStkPop .:= True

updateV :: Offset Log2MaxPush -> Vec CMaxPush (Maybe Atom) -> CPU ()
updateV off as =
  vStkOut .:= Just (off, as ++ repeat Nothing) >>
  case head as of
    Just t -> top' .= t
    Nothing -> return ()
  -- Update top' state to inform memory prefetching in `defaultOutput`
