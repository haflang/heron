{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE TypeFamilyDependencies #-}

{-| A simple stack implementation -}
module Heron.Core.Stack
 ( -- * Generation
   newStack
 , -- * Stack Types
   RamAddr
 , Pop
 , SIn
 , SOut (..)
 , Stack
 ) where

import           Clash.Prelude
import           Data.Maybe       (isJust)
import           Heron.Core.Types

-- | Stack's RAM address type indexed by the depth
type RamAddr d = Index d
-- | Stack's pop type
type Pop = Bool

-- | Stack input is an @a@ value to push and a pop flag
type SIn a
  = (Maybe a, Pop)
  -- ^ (Push word, Pop flag)

-- | Stack output contains current size, current top element, and the new top
-- element after any push/pops.
data SOut a d
  = SOut
  { _size   :: RamAddr d -- ^ Current size of the stack
  , _top    :: Maybe a   -- ^ Current top stack element
  , _newTop :: Maybe a   -- ^ New top stack element after pending operation
  } deriving (Show, Generic, NFDataX, ShowX)

instance SizedRead (SOut a d) where
  type SizedAddr (SOut a d) = RamAddr d
  type SizedData (SOut a d) = Maybe a
  size (SOut sz _ _) = sz
  read (SOut _  x _) = x

-- | A stack contains elements of type @a@ with a depth of @d@ elements and a
--   pop-before-push semantics.
type Stack dom a d = Signal dom (SIn a) -> Signal dom (SOut a d)

-- | Single-port memory primitive to use in stack architecture
type RamPrim dom a d
  =  Signal dom (RamAddr d)
  -> Signal dom (Maybe (RamAddr d, Maybe a))
  -> Signal dom (Maybe a)

-- | Construct a stack using the given memory primitive
newStack
  :: forall d a dom .
     ( KnownNat d
     , NFDataX  a
     , 1 <= d
     , HiddenClockResetEnable dom )
  => RamPrim dom a d -> Stack dom a d
newStack ramPrim inps = SOut <$> sp <*> top <*> top'
  where
    -- Unpack inputs
    (mpush, pop) = unbundle inps

    -- Stack size tracking
    applyOffset sz doPop doPush =
      case (doPop, doPush) of
        (False, False) -> sz
        (False, True ) -> sz+1 -- Infer two parallel adders rather than two cascaded
        (True , False) -> sz-1
        (True , True ) -> sz
    sp  = delay (0 :: RamAddr d) sp'
    sp' = applyOffset <$> sp <*> pop <*> fmap isJust mpush

    -- Generate RAM inputs
    ramWrite = mux ((isJust <$> mpush) .&&. (not <$> pop))
                     (Just <$> bundle (sp',top))
                     (pure Nothing)
    ramRead  = sp'

    -- Gather RAM outputs
    ramOut = readNew ramPrim
                     ramRead
                     ramWrite

    -- Pick next top element
    top' = mux (isJust <$> mpush)
                 mpush
                 (mux pop ramOut top)

    top = delay Nothing top'
