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
  , heapReadSingle
  , heapWriteSingle
  , heapAllocSingle
  , heapReadN
  , heapWriteN
  , heapAllocN
  , heapNoOp
  ) where

import Clash.Prelude

import Heron.Core.Types

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

-- | A heap is a multi-ported memory. A `Heap a p d` has elements of type `a`, `p`
--   independent ports, and a depth of `d` elements.
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
getWriteAddr _ = Nothing

-- Helper functions to construct input controls
heapNoOp
  :: forall p d a .
     ( KnownNat p
     , KnownNat d
     , NFDataX  a
     )
  => HeapIn a p d
heapNoOp = repeat RamNoOp

heapReadSingle
  :: forall p d a .
     ( KnownNat p
     , KnownNat d
     , NFDataX  a
     , 1 <= p
     )
  => RamAddr d -> HeapIn a p d
heapReadSingle addr = leToPlus @1 @p $
                      (RamRead addr) :> repeat RamNoOp

heapReadN
  :: forall p d a n .
     ( KnownNat p
     , KnownNat d
     , KnownNat n
     , NFDataX  a
     , n <= p
     )
  => Vec n (RamAddr d) -> HeapIn a p d
heapReadN addrs = leToPlus @n @p $
                  map RamRead addrs ++ repeat RamNoOp

heapWriteSingle
  :: forall p d a .
     ( KnownNat p
     , KnownNat d
     , NFDataX  a
     , 1 <= p
     )
  => (RamAddr d, a) -> HeapIn a p d
heapWriteSingle write = leToPlus @1 @p $
                        (uncurry RamWrite write) :> repeat RamNoOp

heapWriteN
  :: forall p d a n.
     ( KnownNat p
     , KnownNat d
     , KnownNat n
     , NFDataX  a
     , n <= p
     )
  => Vec n (RamAddr d, a) -> HeapIn a p d
heapWriteN writes = leToPlus @n @p $
                    map (uncurry RamWrite) writes ++ repeat RamNoOp

heapAllocSingle
  :: forall p d a .
     ( KnownNat p
     , KnownNat d
     , NFDataX  a
     , 1 <= p
     )
  => RamAddr d      -- Current heap pointer
  -> a              -- Object to write
  -> ( RamAddr d    -- Allocated address
     , HeapIn a p d -- Heap controls
     )
heapAllocSingle hp x = (addr , heapWriteSingle write)
  where
    addr = hp
    write = (addr, x)

heapAllocN
  :: forall p d a n .
     ( KnownNat p
     , KnownNat d
     , KnownNat n
     , NFDataX  a
     , n <= p
     )
  => RamAddr d            -- Current heap pointer
  -> Vec n a              -- Objects to write
  -> ( Vec n (RamAddr d)  -- Allocated addresses
     , HeapIn a p d     ) -- Heap controls
heapAllocN hp xs = (addrs , heapWriteN writes)
  where
    addrs = iterateI (+1) hp
    writes = zip addrs xs
