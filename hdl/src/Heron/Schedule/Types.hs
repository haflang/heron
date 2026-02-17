module Heron.Schedule.Types where

import           Clash.Prelude
import           Control.Lens     hiding (Index, assign, at, both, imap,
                                   modifying, op, (:>))
import           Heron.Error
import           Heron.Parameters
import           Heron.Template

data PortId
  = PUp
  | PRight
  | PDown
  | PLeft
  deriving (Generic, NFDataX, Show, ShowX, Eq, BitPack)

type DstAddr = GlobalAddr
type SrcAddr = GlobalAddr
type ReadPort = Maybe ComMsg
type Incr = Bool

data Workload = Workload
  { wkId   :: CoreId             -- ^ Core Id of estimated max load PE
  , wkHop  :: Index (HopCount+1) -- ^ Current distance from max load PE
  , wkLoad :: HeapAddr           -- ^ Size of PE's spark pool
  }
  deriving (Generic, NFDataX, Show, ShowX, Eq, BitPack, AutoReg)

-- | Messages passed scheduler cores
data ComMsg
  = MFish SrcAddr (Unsigned (CLog 2 MaxCores))
  -- ^ A core sends a fishing message when it's idle and looking for work. The
  -- response is either a `MFizzled` if the top spark is already under
  -- evaluation/in WHNF, or `MPut` for a successful spark.
  | MFizzled DstAddr
  -- Failed fish
  | MFetch DstAddr TSOAddr SrcAddr
  -- ^ Request that the node stored at `DstAddr` be moved (or copied if its
  -- WHNF) to the new address `SrcAddr`. If the owner of this data made an indirection, the receiver will need to increment their reference count.
  | MPut Incr DstAddr SrcAddr HeapNode
  -- ^ Response to a `MFetch` containing the remote node data, tagged with the CoreId of the source
  | MSuspend DstAddr (Maybe GlobalAddr) -- Suspend the given TSO with an optional blockee linked list tail pointer
  | MUnblock DstAddr
  | MCollected DstAddr
  -- ^ One external reference to an address has been deallocated. The owner
  -- should update its GC records.
  | MFail CoreId Err
  -- ^ Failure, halts the master core. Currently only signals heap exhaustion
  deriving (Generic, NFDataX, Show, ShowX, Eq, BitPack, AutoReg)

-- | Mutator -> Scheduler messages
data MutComRequest
  = MCFetch TSOAddr SrcAddr HeapAddr -- NB Mutator doesn't need to know its own core ID!
  | MCCollected GlobalAddr
  | MCFish HeapAddr
  | MCLocalSuspend TSOAddr (Maybe GlobalAddr)
  | MCFail Err
  deriving (Generic, NFDataX, Show, ShowX, Eq, BitPack)

-- | Scheduler -> Mutator messages
data ComMutRequest
  = CMPut Bool Incr CoreId HeapNode -- ^ With flag saying if addresses must be resolved to local addresses or not
  | CMLocalSpark HeapAddr
  | CMPause
  | CMFizzled
  | CMResume TSOAddr
  | CMSuspend (Maybe GlobalAddr) -- ^ Tail pointer for blockee linked list
  | CMFail CoreId Err
  deriving (Generic, NFDataX, Show, ShowX, Eq, BitPack)

-- | Requests for the Thread manager
data ThreadCmd
  -- Mut only
  = PopReady             -- Atomic, ready head already visible
  | PushSpark HeapAddr   -- Atomic, no return
  -- GC only
  | PeekBlocked          -- Atomic, returns Maybe TSOAddr
  | PeekReady            -- Atomic, returns Maybe TSOAddr
  | PeekSpark            -- Atomic, returns Maybe HeapAddr
  | CollectSpark         -- Atomic, no return
  -- Com only
  | PopSpark             -- Atomic, sp head already visible
  | Unblock TSOAddr      -- Complex, returns ()
                         -- Removes TSO from blocks by traversal, pushes onto ready
  | PushBlock TSOAddr    -- Atomic, no return
  -- Mut and com shared
  | FizzleSpark HeapAddr -- Atomic, no return
  deriving (Generic, NFDataX, Show, ShowX, Eq, BitPack)

data ThreadResult
  = TsoElem   Bool (Maybe TSOAddr) -- ^ TSO list element, tagged with "last" flag. Empty lists return TsoElem True Nothing
  | SparkElem Bool (Maybe (HeapAddr, ThreadTag))
  deriving (Generic, NFDataX, Show, ShowX, Eq, BitPack)

data ThreadTag
  = Sparked
  | Fizzled
  | Ready
  | Blocked
  deriving (Generic, NFDataX, Show, ShowX, Eq, BitPack)

data RefCountCmd
  = RCModify (Maybe (Maybe GlobalAddr)) (Signed 2) HeapAddr -- Only COM needs +1 while both COM and GC can -1, so -2 to +1 range is fine.
  | RCTraverse
  | RCFree -- Frees the node that was most recently read via RCTraverse TODO Do we want to block on result, or are we happy to be async?
  deriving (Generic, NFDataX, Show, ShowX, Eq, BitPack)

data RefCountNode = RefCountNode
  { _rcSrc   :: Maybe GlobalAddr
  , _rcCount :: RefCount
  , _rcTail  :: Maybe HeapAddr
  }
  deriving (Generic, NFDataX, Show, ShowX, BitPack)
makeLenses ''RefCountNode

-- * Port helpers

popRead :: PortId -> Vec 4 Bool
popRead p = drivePort p True False

drivePort :: PortId -> a -> a -> Vec 4 a
drivePort p t f = replace (portIndex p) t $ replicate d4 f

portIndex :: PortId -> Index 4
portIndex PUp    = 0
portIndex PRight = 1
portIndex PDown  = 2
portIndex PLeft  = 3

portIncr :: PortId -> PortId
portIncr PUp    = PRight
portIncr PRight = PDown
portIncr PDown  = PLeft
portIncr PLeft  = PUp

portIds :: Vec 4 PortId
portIds = PUp :> PRight :> PDown :> PLeft :> Nil

portsRead :: PortId -> Vec 4 ReadPort -> (PortId, Maybe (PortId, ComMsg))
portsRead offset rds = (offset', rd)
  where
    offset' = portIncr offset
    rd = fold (<|>) $ rotateRight (zipWith (\p r -> (p,) <$> r) portIds rds) (portIndex offset)
    -- TODO We should prioritise reading _responses_ over _requests_ to help avoid protocol-level deadlocks.
    -- That happens when a port buffer is full of requests, but we need to send a response in order to free up space.

portsPush :: PortId -> ComMsg -> Vec 4 (Maybe ComMsg)
portsPush p msg = replace (portIndex p) (Just msg) (replicate d4 Nothing)
