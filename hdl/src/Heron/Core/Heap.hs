{-# OPTIONS_GHC -fno-warn-orphans #-} -- For our orphan Show instance for Clash's `RamOp`

{-| A heap memory with up to two independent ports.
-}
module Heron.Core.Heap
  (
  -- * Generation
    newHeap
  -- * Heap Types
  , Heap
  , RamPrim
  , RamAddr
  , HeapIn
  , HeapOut (..)
  -- * Helpers
  , arbitrateHeap
  ) where

import           Clash.Prelude
import           Heron.Core.Types
import           Heron.Error

-- | Heap address type
type RamAddr d = Index d

-- | Heap control inputs
type HeapIn a p d
  = Vec p (RamOp d a)
  -- ^ Vector of independent controls for each port. `RamOp` is a Clash type for Write, Read, or NoOp.

-- | Heap outputs
data HeapOut a p d
  = HeapOut
  { _size  :: RamAddr d -- ^ Current size of heap memory
  , _size' :: RamAddr d -- ^ New size of heap memory after pending operations
  , _reads :: Vec p a   -- ^ Words returned by each port command
  } deriving (Show, Generic, NFDataX, ShowX)
deriving instance (KnownNat d, KnownNat p, BitPack a, 1<=d)
  => BitPack (HeapOut a p d)

instance SizedRead (HeapOut a p d) where
  type SizedAddr (HeapOut a p d) = RamAddr d
  type SizedData (HeapOut a p d) = Vec p a
  size (HeapOut sz _ _) = sz
  {-# INLINE size #-}
  read (HeapOut _ _ x) = x
  {-# INLINE read #-}

deriving instance ShowX a => ShowX (RamOp d a)
deriving instance (KnownNat d, BitPack a, 1<=d) => BitPack (RamOp d a)

-- | A heap is a multi-ported memory. A @Heap a p d@ has elements of type @a@, @p@
--   independent ports, and a depth of @d@ elements.
--
--   For simple FPGA implementation, we constrain ports to \( \leq 2 \). We
--   could alleviate this with extra multipumping strategies.
type Heap dom a p d = Signal dom (HeapIn a p d) -> Signal dom (HeapOut a p d)

-- | Memory primitive to compose into a large heap memory. Usually BRAM or UltraRAM.
type RamPrim dom a d
  =  Signal dom (RamOp d a)
  -- ^ RAM operation for port A
  -> Signal dom (RamOp d a)
  -- ^ RAM operation for port B
  -> (Signal dom a, Signal dom a)
  -- ^ Port read outputs

-- | Construct a new heap memory, using a given primitive resource.
newHeap
  :: forall p d a dom .
     ( KnownNat p
     , KnownNat d
     , NFDataX  a
     , HiddenClockResetEnable dom
     , p <= 2
     , 1 <= d
     )
  => RamPrim dom a d -> Heap dom a p d
newHeap ramPrim ops = HeapOut <$> hp <*> hp' <*> bundle ramOuts
  where
    -- Pad input fields to `Vec 2`s
    ops' = unbundle ops ++ replicate (SNat @(2-p)) (pure RamNoOp)
    opA = d0 `at` ops'
    opB = d1 `at` ops'

    -- Heap pointer tracking
    hp  = register (0 :: RamAddr d) hp'
    hp' = max3 <$> opA <*> opB <*> hp
    max3 a b old = max (maybe 0 (1+) $ getWriteAddr a) $
                   max (maybe 0 (1+) $ getWriteAddr b)
                       old

    -- Instantiate dual-port RAM
    (outA, outB) = ramPrim opA opB
    ramOuts = leToPlus @p @2 $
              take (SNat @p) $ outA :> outB :> Nil

getWriteAddr :: KnownNat n => RamOp n a -> Maybe (Index n)
getWriteAddr (RamWrite addr _) = Just addr
getWriteAddr _                 = Nothing

-- | Arbitrates heap access between mutator, scheduler, and collector. The collector and scheduler
-- operations are always scheduled on the first heap port, and priority is always
-- given to the mutator (followed by the scheduler).
arbitrateHeap
  :: forall d a p .
     ( KnownNat p
     , KnownNat d
     , NFDataX  a
     , Show     a
     )
  => HeapIn a (p+2) d
  -> RamOp d a
  -> RamOp d a
  -> (HeapIn a (p+2) d, Bool, Bool, Maybe Err)
arbitrateHeap ops gc com
  | mutIdle && isOp com = go (com :> tail ops) False True
  | mutIdle && isOp gc  = go (gc  :> tail ops) True  False
  | otherwise           = go ops               False False
  where
    go xs a b = (xs, a, b, checkCollision xs)
    mutIdle = isNoOp $ head ops
    isOp = not . isNoOp
    isNoOp RamNoOp = True
    isNoOp _       = False
    checkCollision :: HeapIn a (p+2) d -> Maybe Err
    checkCollision ((RamWrite x _) :> (RamWrite y _) :> _)
      | x==y = Just ErrMemCollisionWW -- RF BRAM only has W-W collisions
{-
    checkCollision as@((RamRead x) :> (RamWrite y _) :> zs)
      | x==y = errorX $ unwords ["Possible RW heap collision on ", show as] -- RF BRAM only has W-W collisions
    checkCollision as@((RamWrite x _) :> (RamRead y) :> zs)
      | x==y = errorX $ unwords ["Possible WR heap collision on ", show as] -- RF BRAM only has W-W collisions
-}
    checkCollision _ = Nothing
-- TODO Really need to specialise the collision checking for each memory, taking into account our invariants.
