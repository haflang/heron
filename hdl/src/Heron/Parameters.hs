{- | The global configuration for the Heron Core's parameters. These are set at
   compile time to keep the main sources a bit cleaner, but it does reduce the
   type-level guarantees about the parameter space.
-}

{-# LANGUAGE FlexibleInstances #-}

module Heron.Parameters
  (
  -- * User settings
  -- ** Template Dimensions
    MaxAps
  , MaxPush
  , NodeLen
  , MaxRegs
  , MaxApSpan
  , MaxFnAps

  -- ** Memory Depths
  , VStkSize
  , AStkSize
  , UStkSize
  , PStkSize
  , HeapSize
  , RomSize
  , GCMutBufSize
  , PortBufSize

  -- ** Field Widths
  , TagW
  , IntW
  , ShortTagW
  , ShortIntW
  , MaxArgs
  , RefCountW

  -- ** Non-functional circuit properties
  , ClkT
  , KnownHeapArch
  , specialiseHeap
  , heapConfig

  -- ** Multicore settings
  , MeshCols
  , MeshRows
  , Meshes

  -- * Derived settings
  -- ** Stack config
  , CMaxPush
  , Log2MaxPush
  , MaxPECols
  , MaxPERows
  , HopCount
  , MaxCores
  ) where

import           Clash.Prelude

-- | Maximum applications per template
type MaxAps = 2
-- | Maximum length for template's heap applications
type NodeLen = 4
-- | Maximum length for template's spinal application
type MaxPush = 6

-- | Maximum number of primitive registers per core
type MaxRegs = 2
-- | Maximum distance between an application and its reference (we need to buffer most-recently allocated addresses)
type MaxApSpan = 16

---- Memory depths
-- | Depth of primary /v/alue stack
type VStkSize = 8192
-- | Depth of case table /a/lternatives stack
type AStkSize = 4096
-- | Depth of /u/pdate pointer stack
type UStkSize = 4096
-- | Depth of /p/pdate stack
type PStkSize = 1024
-- | Depth of heap memory
type HeapSize = 8*1024
  -- ^ Reduced to 8k so we can fit atoms in 18-bit BRAMs with new `PtrTag`s
-- | Size of the GC's Mutation Buffer for handling updates
type GCMutBufSize = 16
-- | Depth of template memory
type RomSize = 1024
-- | Maximum applications across split templates. Informs how many addresses we
-- reserve on the free list before counting the heap as dangerously full
type MaxFnAps = 60 --90

---- Field widths
-- | Constructor tag width
type TagW = 10
-- | Primitive integer width
type IntW = 15
-- | Short constructor tag width (for inline case tables)
type ShortIntW = 6
-- | Short primitive integer width (for inline case tables)
type ShortTagW = 2
-- | Maximum arity for any template
type MaxArgs = 7
-- | Reference Counts
type RefCountW = 15

-- | Number of Heron cores
type MeshCols = 2
type MeshRows = 4
type Meshes   = 1
type PortBufSize = 16

type MaxPERows = MeshRows * Meshes
type MaxPECols = MeshCols
type HopCount = MaxPERows + MaxPECols - 2
type MaxCores  = MaxPERows * MaxPECols

-- * Circuit's non-functional properties

-- | Target clock period in picoseconds
type ClkT = 10000 -- 100 MHz -- 5882 -- 170 MHz

data MemoryArch = UltraRAM | BlockRAM
data HeapArch (a :: MemoryArch) = MkHeapArch

{- | Heap memory configuration. There are a few points in the circuit where the
     logic changes depending on subtle properties on the heap memory
     architecture. For example, when using UltraRAM, we can use two ports to
     read-before-write since it is multi-pumped. If using BlockRAM, we can use
     READ_FIRST mode instead.

     We define this configuration as a singleton `HeapArch` so our circuit
     description can be generalised for all heap architectures while
     (hopefully!) ensuring it is entirely specialised away during type erasure.
     I'm not sure I'd trust the compiler to do the partial evaluation if
     everything is at the term-level.
-}
class KnownHeapArch h where
  -- | Select an argument based on a `HeapArch` singleton. This lets us
  -- specialise behaviour for BlockRAM and UltraRAM heaps with a little more
  -- confidence that the netlist will only contain /one/ option.
  specialiseHeap :: h -- ^ Architecture for specialisation
                 -> a -- ^ Function for UltraRAMs
                 -> a -- ^ Function for BlockRAMs
                 -> a
instance KnownHeapArch (HeapArch 'UltraRAM) where
  specialiseHeap _ a _ = a
instance KnownHeapArch (HeapArch 'BlockRAM) where
  specialiseHeap _ _ a = a

-- | Heap memory architecture
heapConfig :: HeapArch 'UltraRAM
heapConfig  = MkHeapArch

-- * Derived settings

-- | Synonym for \( \lceil\log_2 \mathtt{MaxPush} \rceil \), used in efficient power-of-two
--   parallel stack implementation
type Log2MaxPush = CLog 2 MaxPush

-- | Synonym for \( 2^{\lceil\log_2 \mathtt{MaxPush} \rceil} \), used for efficient power-of-two
--   parallel stack implementation
type CMaxPush    = 2 ^ Log2MaxPush
