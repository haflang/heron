{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE TypeFamilyDependencies #-}

{-| A simple stack implementation -}
module Heron.Core.Fifo
 ( -- * Generation
   newFifo
 , -- * Stack Types
   RamAddr
 , Pop
 , FIn
 , FOut (..)
 , Fifo
 ) where

import           Clash.Prelude
import           Data.Maybe       (isJust)
import           Heron.Core.Types

-- | Fifo's address type indexed by depth
type RamAddr d = Unsigned (CLog 2 d)

-- | Fifo's pop type
type Pop = Bool

-- | Fifo input is an @a@ value to enqueue and a pop flag
type FIn a
  = (Maybe a, Pop)

-- | Fifo output contains buffer size and the head element
data FOut a d
  = FOut
  { _size :: RamAddr d -- ^ Current size of the fifo
  , _next :: Maybe a   -- ^ Current head element of the fifo
  } deriving (Show, Generic, NFDataX, ShowX)

instance SizedRead (FOut a d) where
  type SizedAddr (FOut a d) = RamAddr d
  type SizedData (FOut a d) = Maybe a
  size (FOut sz _) = sz
  read (FOut _  x) = x

-- | A fifo contains elements of type @a@ with a depth of @d@ elements
type Fifo dom a d = Signal dom (FIn a) -> Signal dom (FOut a d)

-- | Single-port memory primitive to use in fifo architecture
type RamPrim dom a d
  =  Signal dom (RamAddr d)
  -> Signal dom (Maybe (RamAddr d, Maybe a))
  -> Signal dom (Maybe a)

-- | Construct a fifo using the given memory primitive
newFifo
  :: forall d a dom .
     ( KnownNat d
     , NFDataX  a
     , 1 <= d
     , HiddenClockResetEnable dom )
  => RamAddr d -> RamPrim dom a d -> Fifo dom a d
newFifo initWrp ramPrim inps = FOut <$> sz <*> top
  where
    -- Unpack inputs
    (mpush, pop) = unbundle inps

    -- Stack size tracking
    rdp  = delay (0 :: RamAddr d) rdp'
    wrp  = delay initWrp          wrp'
    rdp' = mux (pop .&&. sz ./=. 0) (rdp + 1) rdp
    wrp' = mux (isJust <$> mpush) (wrp + 1) wrp
    sz   = wrp - rdp

    -- Generate RAM inputs
    ramWrite = liftA2 (\x y -> fmap (const (y, x)) x) mpush wrp

    -- Gather RAM outputs
    top = readNew ramPrim rdp' ramWrite
