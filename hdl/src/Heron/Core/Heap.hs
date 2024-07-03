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
  , arbitrateGC
  ) where

import           Clash.Prelude

import           Heron.Core.Types

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

instance SizedRead (HeapOut a p d) where
  type SizedAddr (HeapOut a p d) = RamAddr d
  type SizedData (HeapOut a p d) = Vec p a
  size (HeapOut sz _ _) = sz
  read (HeapOut _ _ x) = x

deriving instance ShowX a => ShowX (RamOp d a)

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

-- | Arbitrates heap access between mutator and collector. The collector
-- operation is always scheduled on the first heap port, and priority is always
-- given to the mutator.
arbitrateGC
  :: forall d a p .
     ( KnownNat p
     , KnownNat d
     , NFDataX  a
     )
  => HeapIn a (p+1) d
  -> RamOp d a
  -> (HeapIn a (p+1) d, Bool)
arbitrateGC ops RamNoOp = (ops, False)
arbitrateGC ops gc
  | not (isNoOp $ head ops) = (ops, False) -- error "GC and Core conflict on port A" --DEBUG
  -- | any (collides gc) ops   = error "GC READ and Core WRITE conflict" --DEBUG
  | otherwise = (gc :> tail ops, True)
  where
    isNoOp RamNoOp = True
    isNoOp _       = False
    -- collides (RamRead x) (RamWrite y _) = x==y
    -- collides _ _ = False
