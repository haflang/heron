{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-} -- For unused generated lenses

{-| The Heron Core's control logic. This captures the core's combinatorial logic
  for performing reductions and controlling execution phases (reduction, GC,
  stalls, etc.).

  The style is heavily inspired by [Gergő Érdi's Retrocomputing with Clash
  book](https://erdi.dev/retroclash/). Use of @barbies@ is to let us use just
  /one/ record type for both combinatorial logic and collections of synchronous
  signals.
-}
module Heron.Core.Core
  ( CPUIn(..)
  , CPUOut(..)
  , CPUState(..)
  , CPUStats(..)
  , Phase(..)
  , Update(..)
  , Mutation(..)
  , cpu
  ) where

import           Barbies.TH
-- import           Clash.Annotations.BitRepresentation.Deriving
import           Clash.Prelude        hiding (fail, read)
import           Control.Lens         hiding (Index, assign, at, imap, op, (:>))
import           Control.Monad.State  hiding (fail)
import           Data.Maybe           (fromJust, fromMaybe, isJust)
import qualified Prelude              as P
import           RetroClash.Barbies
import           RetroClash.CPU       hiding (update)

import           Heron.Core.Alu
import           Heron.Core.Collector (GCCmd (..), GCRequest (..))
import           Heron.Core.Heap
import           Heron.Core.ParStack
import           Heron.Core.Rom
import           Heron.Core.Stack
import           Heron.Core.Types
import           Heron.Error
import           Heron.Parameters
import           Heron.Schedule.Types
import           Heron.Template
import           Heron.TraceFSM

-- CPU data types

data Mutation
  = Enter   HeapAddr HeapNode -- ^ We've unwound a node and replaced it with a lock (GC might need to remember)
  | Modify  HeapAddr          -- ^ We've written to an address (usually overwritting a dummy node we allocated earlier; COM needs to be aware in case its also trying to read it)
  | Release HeapAddr          -- ^ We've commited a WHNF, overwriting a lock (COM needs to handle unblocking)
                              --   Expects a read on port A
  -- NB: No need for an Allocation constructor --- there's nothing extra to do in that case
  deriving (Show, ShowX, Generic, NFDataX, BitPack, Eq, AutoReg)

-- | CPU input record. Includes outputs from all memory components, and a
-- `begin` trigger.
declareBareB [d|
  data CPUIn = CPUIn
    { uStkIn :: SOut    Update                     UStkSize
    -- ^ Size and head of update stack
    , aStkIn :: SOut    (CaseTable UnpackedAlt   ) AStkSize
    -- ^ Size and head of case alternative stack
    , pStkIn :: SOut    PInt                       PStkSize
    -- ^ Size and head of primitive alternative stack
    , vStkIn :: PSOut   Atom      Log2MaxPush      VStkSize
    -- ^ Size and head of parallel value stack
    , heapIn :: HeapOut HeapNode  MaxAps           HeapSize
    -- ^ Applications read from heap ports (may be `undefined` after no-op)
    , tmplIn :: RomOut  Template
    -- ^ Template read from program memory
    , nextAddrs :: Vec MaxAps HeapAddr
    -- ^ Next free addresses for allocation
    , gcCmd     :: GCCmd
    -- ^ Command requested from GC core. Required to stall mutator when heap is
    -- full or ask the mutator to dump the graph roots when starting a GC pass.
    -- ^ Size and head of parallel value stack
    , schedCmd  :: Maybe ComMutRequest
    , schedPause :: Bool
    -- ^ A request made by the scheduler
    , begin     :: Maybe   Atom
    -- ^ Start reduction of main function
    , unlock :: Bool
    , readyPoolNext :: Maybe TSOAddr
    , coreId :: CoreId
    , rcReady :: Bool
    } |]
deriving instance Generic (Pure CPUIn)
deriving instance NFDataX (Pure CPUIn)
deriving instance BitPack (Pure CPUIn)
instance Show (Pure CPUIn) where
  show CPUIn{..} = unlines
    [ ""
    , "  uStk = " P.++ show uStkIn
    , "  aStk = " P.++ show aStkIn
    , "  pStk = " P.++ show pStkIn
    , "  vStk = " P.++ show vStkIn
    , "  heap = " P.++ show heapIn
    , "  tmpl = " P.++ show tmplIn
    , "  fls  = " P.++ show nextAddrs
    , "  cReq = " P.++ show schedCmd
    , "  go   = " P.++ show begin
    ]
instance ShowX (Pure CPUIn) where
  showX CPUIn{..} = unlines
    [ ""
    , "  uStk = " P.++ showX uStkIn
    , "  aStk = " P.++ showX aStkIn
    , "  pStk = " P.++ showX pStkIn
    , "  vStk = " P.++ showX vStkIn
    , "  heap = " P.++ showX heapIn
    , "  tmpl = " P.++ showX tmplIn
    , "  fls  = " P.++ showX nextAddrs
    , "  cReq = " P.++ showX schedCmd
    , "  go   = " P.++ showX begin
    ]

-- | Statistics collected from mutator runtime. These allow us to distinguish
-- mutator cycles from GC overhead.
data CPUStats = CPUStats
  { _mutCycles    :: Unsigned 32
  -- ^ Cycles spent doing useful mutation
  , _gcRootCycles :: Unsigned 32
  -- ^ Paused cycles spent dumping graph roots
  , _gcWaitCycles :: Unsigned 32
  -- ^ Paused cycles spent waiting for more free addresses
  , _maxStall     :: Unsigned 32
  -- ^ Worst-case pause time
  , _ctxCycles    :: Unsigned 32
  } deriving (Show, ShowX, Generic, NFDataX, BitPack, Eq, AutoReg)
makeLenses ''CPUStats

initCPUStats :: CPUStats
initCPUStats = CPUStats 0 0 0 0 0

-- | Current CPU execution phase
data Phase
  = Init      -- ^ Initialising
  | AllocTSO  -- ^ Reserve addresses for a new TSO and graph root
  | Starting  -- ^ Waiting for initialisation
  | Reduce    -- ^ Performing reductions
  | Stall     -- ^ Stalling until heap port becomes available
  | GCStall   -- ^ Stalling until heap space is freed
  | Halt      -- ^ Finished reducing
  | DumpRoots -- ^ Dumping GC roots from primary stack
  | ContinueUpdate -- ^ Extra cycle needed for wide updates
  | Fetch     -- ^ Fetching a node held in another core
  | Suspend   -- ^ Save stack state onto the heap with as a TSO
  | Resume    -- ^ Load a TSO back onto the stacks
  deriving (Show, Generic, NFDataX, Enum, Bounded, Eq, ShowX, BitPack)
-- deriveAnnotation (simpleDerivator OneHot OverlapL) [t| Phase |]
-- deriveBitPack [t| Phase |]
-- FIXME The one-hot encoding seems to make clash verilog output fail

-- | CPU output record. Includes inputs for all memory components, and the
-- `_result` node.
declareBareB [d|
  data CPUOut = CPUOut
    { _uStkPush  :: Maybe Update
    -- ^ Push element to update stack
    , _uStkPop   :: Bool
    -- ^ Pop element from update stack
    , _aStkPush  :: Maybe (CaseTable UnpackedAlt)
    -- ^ Push element to case alternative stack
    , _aStkPop   :: Bool
    -- ^ Pop element from case alternative stack
    , _pStkPush  :: Maybe PInt
    -- ^ Push element to primitive stack
    , _pStkPop   :: Bool
    -- ^ Pop element from primitive stack
    , _vStkOut   :: PSIn   Atom      Log2MaxPush
    -- ^ Pop _and_ push to parallel value stack
    , _heapOut   :: HeapIn HeapNode  MaxAps         HeapSize
    -- ^ Heap operations
    , _tmplOut   :: RomIn  RomSize
    -- ^ Program memory operation
    , _upd :: Maybe Mutation
    -- ^ Signal that a node has been mutated
    , _gcRequest     :: GCRequest
    -- ^ Graph roots for the GC snapshot (any heap pointer on the main value stack)
    , _allocBubble :: Bool
    -- ^ A flag raised when we are certain no allocations will happen on the next cycle
    , _schedRequest :: Maybe MutComRequest
    , _result    :: (CPUStats, Maybe Atom)
    -- ^ The final result. Will return single-atom NFs directly, or a heap
    -- pointer to larger WHNFs.
    , _mutPhase :: Phase
    , _mutThreadCmd :: Maybe ThreadCmd
    , _schedCmdPop  :: Bool
    , _rcReq :: Maybe RefCountCmd
    , _addrTSO :: HeapAddr
    , _errMut :: Maybe Err
    } |]
makeLenses ''CPUOut
deriving instance Generic (Pure CPUOut)
deriving instance NFDataX (Pure CPUOut)
deriving instance BitPack (Pure CPUOut)

instance Show (Pure CPUOut) where
  show CPUOut{..} = unlines
    [ ""
    , "  uStk  = " P.++ show _uStkPush P.++ " " P.++ show _uStkPop
    , "  aStk  = " P.++ show _aStkPush P.++ " " P.++ show _aStkPop
    , "  pStk  = " P.++ show _pStkPush P.++ " " P.++ show _pStkPop
    , "  vStk  = " P.++ show _vStkOut
    , "  heap  = " P.++ show _heapOut
    , "  tmpl  = " P.++ show _tmplOut
    , "  gcReq = " P.++ show _gcRequest
    , "  upd   = " P.++ show _upd
    , "  phase = " P.++ show _mutPhase
    , "  comReq = " P.++ show _schedRequest
    , "  res   = " P.++ show _result
    ]
instance ShowX (Pure CPUOut) where
  showX CPUOut{..} = unlines
    [ ""
    , "  uStk  = " P.++ showX _uStkPush P.++ " " P.++ showX _uStkPop
    , "  aStk  = " P.++ showX _aStkPush P.++ " " P.++ showX _aStkPop
    , "  pStk  = " P.++ showX _pStkPush P.++ " " P.++ showX _pStkPop
    , "  vStk  = " P.++ showX _vStkOut
    , "  heap  = " P.++ showX _heapOut
    , "  tmpl  = " P.++ showX _tmplOut
    , "  gcReq = " P.++ showX _gcRequest
    , "  upd   = " P.++ showX _upd
    , "  phase = " P.++ showX _mutPhase
    , "  comReq = " P.++ showX _schedRequest
    , "  res   = " P.++ showX _result
    ]

data FetchOp
  = FOWait     CoreId     -- ^ Waiting for a response from remote core
  | FOUnpack   CoreId Bool (Vec NodeLen (Maybe HeapAddr)) -- ^ Expand new FetchMe nodes for any children and commit main node. Holds a vector of outstanding addresses for resolution.
  deriving (Show, ShowX, Generic, NFDataX, BitPack, Eq)

type Cooldown = Unsigned 10
minCooldown :: Cooldown
minCooldown = 8
maxCooldown :: Cooldown
maxCooldown = 512 :: Cooldown
incCooldown :: Cooldown -> Cooldown
incCooldown x = if x >= maxCooldown
                   then maxCooldown
                   else x `shiftL` 1

data CPUState = CPUState
  { _phase         :: Phase
  , _top'          :: Atom
  , _alt'          :: Maybe (CaseTable UnpackedAlt)
  , _regs          :: Vec MaxRegs Atom
  , _frozenArgs    :: Vec CMaxPush Atom
  , _forwardedNode :: Maybe HeapNode
  , _allocs        :: Vec MaxApSpan HeapAddr
  , _cooldown      :: Cooldown
  , _stalls        :: Cooldown
  , _wideUpd       :: (HeapAddr, HeapNode, Bool)
  , _fetching      :: FetchOp
  , _stats         :: CPUStats
  , _curStall      :: Unsigned 32
  , _reservedAddr  :: Maybe HeapAddr
  , _fishCooldown  :: Bool
  , _tsoAddr       :: HeapAddr
  , _blockeeTail   :: Maybe (Maybe GlobalAddr)
  , _prevPhase     :: Phase
  , _forceEntry    :: Bool -- ^ We will unwind node held in `forwardedNode` but it is already locked in the heap, so don't clobber the lock
  }
  deriving (Generic, NFDataX, BitPack, Eq, Show, ShowX, AutoReg)
makeLenses ''CPUState

initCPUState :: CPUState
initCPUState = CPUState
  { _phase = Init
  , _top'  = PrimInt 0
  , _alt'  = Nothing
  , _regs  = repeat (PrimInt 0)
  , _frozenArgs    = repeat (unpack 0)
  , _forwardedNode = Nothing
  , _allocs = repeat undefined
  , _wideUpd = (0, unpack 0, False)
  , _fetching = FOWait (0,0)
  , _cooldown = minCooldown
  , _stalls = 0
  , _stats = initCPUStats
  , _curStall = 0
  , _reservedAddr = Nothing
  , _fishCooldown = True
  , _tsoAddr = 0
  , _blockeeTail = Nothing
  , _prevPhase = Init
  , _forceEntry = False
  }

defaultRead :: Atom -> Vec MaxAps (RamOp HeapSize a)
defaultRead a = case heapAddr a of
  Nothing -> repeat RamNoOp
  Just x  -> repeat RamNoOp ++ singleton (RamRead x)

defaultOutput :: CPUState -> Pure CPUOut
defaultOutput CPUState{..} = CPUOut
  { _uStkPush = Nothing
  , _uStkPop  = False
  , _aStkPush = Nothing
  , _aStkPop  = False
  , _pStkPush = Nothing
  , _pStkPop  = False
  , _vStkOut = Nothing
  , _heapOut = defaultRead _top'      -- Fetch when top is pointer (INV2)
  , _tmplOut = romOp _top' _alt' -- Fetch when top is FUN/CON (INV1)
  , _upd = Nothing
  , _gcRequest = RNothing
  , _schedRequest = Nothing
  , _allocBubble = hasBubble _top'
  , _result  = (_stats, Nothing)
  , _mutPhase = _phase
  , _mutThreadCmd = Nothing
  , _schedCmdPop = False
  , _rcReq = Nothing
  , _addrTSO = _tsoAddr
  , _errMut = Nothing
  }
  where
    romOp  (Fun _ a _) _                              = a
    romOp  (Con _ tag) (Just (CTOffset alt         )) = alt + bitCoerce (resize tag)
    romOp  (Con _ tag) (Just (CTInline (UAFun alt) _))
      | lsb tag == low  = alt
    romOp  (Con _ tag) (Just (CTInline _ (UAFun alt)))
      | lsb tag == high = alt
    romOp  _             _                            = 0
    hasBubble _
      | _phase == Halt      = True
      | _phase == Init      = True
      | _phase == AllocTSO  = True
      | _phase == Starting  = True
      | _phase == Stall     = True
      | _phase == GCStall   = True
      | _phase == DumpRoots = True
      | _phase == ContinueUpdate = False
    hasBubble (Fun {}) = False
    hasBubble (Con {}) = False
    hasBubble _        = True

-- | Synchronous control logic, packaged as a Mealy machine
cpu :: (HiddenClockResetEnable dom)
    => Signal dom CoreId -> Signals dom CPUIn -> Signals dom CPUOut
cpu = traceFSM "_mutator" initCPUState defaultOutput step
{-# NOINLINE cpu #-}

type CPU = CPUM CPUState CPUOut

-- Dispatch for CPU phases

step :: Pure CPUIn -> CPU ()
step ins@CPUIn{..} =
  use phase >>= \case
    Halt   -> do
      s <- use stats
      let ret = head (read vStkIn)
      result .:= (s, Just ret)
      -- Allow transition to dump roots so we can continue GC after a benchmark and see what's left "live"
      -- when gc $ do
      --  phase .= DumpRoots
      --  prevPhase .= Halt
      phase .= Init
      updateV (-1) (repeat Nothing)
      -- ^ Above is for testing. It allows re-entry into the same program. If we rerun the benchmark lots of times and observe no slowdown or heap exhausting, we're pretty sure there are no major space leaks.
      -- TODO Safely broadcast a halt and remove all speculative sparks
      -- gcRequest .:= RFinished
      -- phase   .= Init
      -- stats .= initCPUStats
      -- cooldown .= minCooldown
    Init   -> do
      -- Give some initial values to stack tops
      updateV 0 (repeat $ Just $ Fun 0 0 True)
      stats . ctxCycles %= (+1)
      unless (pause || not unlock) $ do
        phase         .= AllocTSO
        allocBubble  .:= False -- AllocTSO does allocation
    AllocTSO -> do
        phase         .= Starting
        fishCooldown  .= True  -- Give the starting phase one cycle to check the ready list before fishing
        forwardedNode .= Nothing
        stalls        .= 0
        let addr = head nextAddrs
        let tso  = at d1 nextAddrs
        gcRequest .:= RAlloc (True :> True :> repeat False)
        reservedAddr .= Just addr
        tsoAddr .= tso
        updateV 1 (Just (Ptr PShared addr) :> repeat Nothing)
        heapOut .:= RamWrite tso (App False 0 $ repeat Nothing) :>
                    RamWrite addr (Locked Nothing)              :>
                    repeat RamNoOp
    Starting
      | gcCmd == RootsCmd -> do
          stats . gcRootCycles %= (+1)
          switchPhase Starting DumpRoots
      | otherwise -> do
          stats . ctxCycles %= (+1)
          starting gcCmd ins
    Stall -> do
      stats . mutCycles %= (+1)
      stall
    GCStall -> do
      stats . gcWaitCycles %= (+1)
      curStall %= (+1)
      unless pause $ do
        p <- use prevPhase -- TODO Combine these two lines
        phase .= p
        allocBubble .:= False
    Reduce
      | pause -> do
          stats . gcWaitCycles %= (+1)
          switchPhase Reduce GCStall
      | gc    -> do
          stats . gcRootCycles %= (+1)
          switchPhase Reduce DumpRoots
      | otherwise -> do
          latchMaxStall
          curStall .= 0
          stats . mutCycles %= (+1)
          top' .= top vStkIn
          alt' .= _newTop aStkIn
          reduce (top vStkIn) ins
    DumpRoots -> do
      stats . gcRootCycles %= (+1)
      curStall %= (+1)
      dumpRoots ins
    ContinueUpdate -> do
      stats . mutCycles %= (+1)
      continueUpdate ins
    Fetch
      | gcCmd == RootsCmd -> do
          stats . gcRootCycles %= (+1)
          switchPhase Fetch DumpRoots
      -- | pause -> do
      --     stats . gcWaitCycles %= (+1)
      --     switchPhase Fetch GCStall
      -- ^ Above was dangerous... we only do < NodeLen allocations, so all good.
      --   If we do pause during unpacking, we leave some fetchme nodes dangling
      --   since the full node is committed at the end
      | otherwise -> do
          stats . ctxCycles %= (+1)
          use fetching >>= fetch (head nextAddrs) schedCmd nextAddrs rcReady
    Suspend ->
      stats . ctxCycles %= (+1) >>
      suspend ins
    Resume ->
      stats . ctxCycles %= (+1) >>
      resume ins
  where
    -- Do we need to pause for COM/GC?
    pause = gcCmd == WaitCmd    ||
            gcCmd == WaitBufCmd ||
            schedPause
    -- Do we need to jump to DumpRoots soon?
    gc    = gcCmd == RootsCmd && canGC (top vStkIn)

failure :: Err -> CPU ()
failure e = errMut .:= Just e >> phase .= Halt

-- Looking for more work
starting :: GCCmd -> Pure CPUIn -> CPU ()
starting gc CPUIn{..} = do
  heapOut   .:= repeat RamNoOp -- Free up heap access for COM/GC
  startAddr  <- fromJust <$> use reservedAddr
  notFishing <- use fishCooldown

  case begin of

    -- User requested work available
    Just root -> do
      let retAddr = fromMaybe startAddr (heapAddr root)
      when notFishing $ do
        stats .= initCPUStats -- Reset counters (ensures benchmarking only counts actual evaluation)
        go retAddr False True
        updateV (if size vStkIn == 0 then 1 else 0)
                (Just (dash root) :> repeat Nothing)
    Nothing       -> do
      let notStalled = not $ gc == WaitCmd || gc == WaitBufCmd
      -- ^ We don't want to jump the gun in removing a thread from the ready list if we're not able to report the update quite yet.
      let readyp = if notFishing && notStalled then readyPoolNext else Nothing
      case readyp of
        -- Previous work is now unblocked
        Just addr -> when (size vStkIn > 0) $ do
          -- ^ If this is the first cycle, let's just allow `reserveAddress` to
          -- initialise TSO and startAddr nodes instead
          cooldown .= minCooldown
          top' .= Ptr PShared addr
          phase .= Resume
          -- Set stalls to 1 as a flag for first iteration
          stalls .= 1
          -- Prefetch root
          heapOut .:= RamNoOp :> RamRead addr :> repeat RamNoOp

        Nothing -> do
          occasionallyFish startAddr
          case schedCmd of
            -- No work available, wait a bit and try again
            Just CMFizzled -> do
              schedCmdPop .:= True
              wait <- use cooldown
              cooldown %= incCooldown
              stalls .= wait
              fishCooldown .= True

            -- New work local work available
            Just (CMLocalSpark addr) -> do
              schedCmdPop .:= True
              cooldown .= minCooldown
              go addr True False
              top' .= Ptr PShared addr
              updateV 0 (Just (Ptr PShared addr) :> repeat Nothing)
              -- Prefetch root
              heapOut .:= RamNoOp :> RamRead addr :> repeat RamNoOp
              -- Make sure the GC marks _this_ address and not just its children, if we're starting after ustk root ID
              upd .:= Just (Enter addr (App False 1 $ Just (Ptr PShared addr) :> repeat Nothing))

            -- New remote work available
            Just (CMPut resolve indr cid node) -> do
              schedCmdPop .:= True
              cooldown .= minCooldown
              go startAddr True False
              updateV 0 (Just (Ptr PShared startAddr) :> repeat Nothing)

              -- Setup for unpacking a remote node
              let oldAddrs =
                    if resolve
                      then map (maybe Nothing heapAddr) (nAtoms $ unpackNode False node)
                      else repeat Nothing
              fetching .= FOUnpack cid indr oldAddrs
              phase   .= Fetch
              -- We now write lock for startAddr in reserveAddr instead. Don't want to overwrite a blockee pointer
              upd          .:= Just (Modify startAddr)
              forwardedNode .= Just (mapNode dash node)
            _ -> return ()
  where
    -- Timer to dispatch Fish messages
    occasionallyFish addr = do
      time <- use stalls
      if time == 0
        then do schedRequest .:= Just (MCFish addr)
                wait <- use cooldown
                stalls .= wait
                fishCooldown .= False
        else do idling <- use fishCooldown
                when idling (stalls .= (time-1))

    -- Generic start operations
    go addr discard isRoot = do
      when isRoot $
        pushU $ Update { uAddr = Just addr
                       , uSp = 1
                       , uDiscard = discard
                       , uRoot = isRoot
                       }
      phase .= Reduce
      allocBubble .:= False
      reservedAddr .= Nothing
      stalls .= 0

-- Dispatch for reduction rules

reduce :: Atom -> Pure CPUIn -> CPU ()
reduce t ins
  | needsUnwind t      = unwind t ins
  | needsUpdate t  ins = update ins
  | needsUnfold t  ins = unfold ins
reduce (Con   _ _) ins = caseSelect ins
reduce (PrimInt _) ins = prim ins
reduce _ _   = failure ErrMutNotRedex

-- Reduction rules

fetch :: HeapAddr -> Maybe ComMutRequest -> Vec MaxAps HeapAddr -> Bool -> FetchOp -> CPU ()
-- Block until we get a `CMPut` message from the schduler
fetch _nextAddr req _ _ (FOWait _) = do
  allocBubble .:= False
  heapOut .:= repeat RamNoOp -- Avoid using heap ports, so scheduler and GC can progress
  case req of
    Just (CMPut resolve indr cid node) -> do
      schedCmdPop .:= True
      forwardedNode .= Just node
      let oldAddrs = if resolve
                       then map (maybe Nothing heapAddr) (nAtoms $ unpackNode False node)
                       else repeat Nothing
      fetching .= FOUnpack cid indr oldAddrs
    Just (CMSuspend tailb) -> do -- Tried to fetch locked node, so we context switch
      -- node <- fromJust <$> use forwardedNode
      -- If we came from a FetchMe node, we need to replace its lock with the original
      -- unless (isLocked node) $ do
      --   addr <- fromJust . heapAddr <$> use top'
      --   heapOut .:= RamRead addr :> RamWrite addr node :> repeat RamNoOp
      --   upd     .:= Just (Release addr)
      schedCmdPop .:= True
      -- forwardedNode .= Nothing
      blockeeTail .= Just tailb
      reservedAddr .= Nothing
      forceEntry .= False
      beginSuspend
    _ -> return ()
-- Handle unpacking of remote node
fetch _ _ as rcReady (FOUnpack cid indr hs) =
  let nextChild = fold (<|>) hs in
  case nextChild of
    Just oldAddr -> do -- More children to consider
      allocBubble .:= False
      when rcReady $ do
        gcRequest .:= RAlloc (True :> repeat False)
        node <- fromJust <$> use forwardedNode
        let newAddr = head as
        -- Remove _all_ instances of oldAddr from our outstanding addr map and
        -- node. This might be costly, but important for maintaining sharing.
            xs = nAtoms $ unpackNode False node :: Vec NodeLen (Maybe Atom)
            (hs',xs') = unzip $ zipWith (\h a -> if h == Just oldAddr then (Nothing, fmap (setHeapAddr newAddr) a) else (h,a)) hs xs
        forwardedNode .= Just (setAtoms node xs')
        -- ^ TODO Can't we dispatch two at once?
        -- If we temporarily assume that NodeLen = 4, we can do one layer of
        -- comparison then return a tuple.
        heapOut   .:= (RamWrite newAddr (FetchMe (cid, oldAddr)) :> repeat RamNoOp)
        -- Doing it this way round prevents collisions with scheduler fetches
        -- (since they are competing for the same port)
        rcReq .:= Just (RCModify (Just $ Just (cid, oldAddr)) 0 newAddr)
        fetching   .= FOUnpack cid indr hs'
    Nothing -> when rcReady $ do -- All childern handled. Commit original node.
      phase .= Reduce
      node <- fromJust <$> use forwardedNode
      addr <- fromJust . heapAddr <$> use top'
      let rc = if indr then 1 else 0
      let ga = case node of
            FetchMe x | indr -> Just x
            _                -> Nothing
      rcReq .:= Just (RCModify (Just ga) rc addr)
      upd .:= Just (Modify addr)
      forceEntry .= True
      -- If we got something that won't be updated later, commit it to heap and release any blockees
      when (isWHNF node) $ do
        heapOut .:= RamRead addr :> RamWrite addr node :> repeat RamNoOp
        upd .:= Just (Release addr)

fetchAddr :: HeapNode -> Maybe GlobalAddr
fetchAddr (FetchMe x) = Just x
fetchAddr _           = Nothing

suspend :: Pure CPUIn -> CPU ()
suspend CPUIn {..}
  -- TODO Do we want to move these guards up into `step`?
  | gcCmd == RootsCmd = switchPhase Suspend DumpRoots
  | gcCmd == WaitCmd || gcCmd == WaitBufCmd = switchPhase Suspend GCStall -- Better to stall to make alloc bubble cycles
  | otherwise = allocBubble .:= False >> use blockeeTail >>= \case

    -- First cycle, so write a valid TSO head
    Just btail -> do
      tso <- use tsoAddr
      fetchGa <- maybe Nothing fetchAddr <$> use forwardedNode
      let n = TSOHead btail fetchGa tso -- Setting circular reference for now --- we'll incrementally update the tail pointer as we construct it.
      heapOut .:= RamWrite tso n :> repeat RamNoOp
      forwardedNode .= Just n
      blockeeTail .= Nothing
    -- Rest, so dump stack states
    Nothing -> do
      let headAddr = head nextAddrs
      tailAddr      <- use reservedAddr
      gcRequest    .:= RAlloc (True :> repeat False)
      reservedAddr  .= Just headAddr
      if suspendStk
        then do -- Save from the main stack
          let as = takeI . imap mask $ read vStkIn
          updateV (negate len') (repeat Nothing)
          let n = TSOPar tailAddr len as
          newHeader <- updTsoHead headAddr
          heapOut .:= RamWrite headAddr n :> newHeader :> repeat RamNoOp
          upd .:= Just (Enter headAddr n)
          when final' $ do
            reservedAddr  .= Nothing
            forwardedNode .= Nothing
            tsoAddr       .= 0
            phase         .= Init -- Find more work
        else do -- Save from the auxillary stacks
          let p = read pStkIn
              c = read aStkIn
              u = read uStkIn
          when (size pStkIn > 0) popP
          when (size uStkIn > 0) popU
          when (size aStkIn > 0) popA
          let n = TSORest tailAddr p c u
          newHeader <- updTsoHead headAddr
          heapOut .:= RamWrite headAddr n :> newHeader :> repeat RamNoOp
          upd     .:= Just (Enter headAddr n)
  where
    suspendStk = size pStkIn == 0 &&
                 size uStkIn == 0 &&
                 size aStkIn == 0
    apLen = SNat @(NodeLen-1)
    final' = size vStkIn <= snatToNum apLen
    len :: Len (NodeLen-1)
    len = resize $ min (snatToNum apLen) (size vStkIn)
    len' :: Offset Log2MaxPush
    len' = bitCoerce (resize len :: Len (2^(1+Log2MaxPush)-1))
    mask i a | i < resize len = Just a
             | otherwise      = Nothing
    updTsoHead a = do
      fromJust <$> use forwardedNode >>= \case
        (TSOHead btail hd _) -> do
          tso <- use tsoAddr
          pure $ RamWrite tso (TSOHead btail hd a)
        _ -> errorX "No TSO header stored in forwardedNode during suspend..."

resume :: Pure CPUIn -> CPU ()
resume CPUIn {..}
  | gcCmd == WaitBufCmd || gcCmd == WaitCmd = switchPhase Resume GCStall
  | otherwise = case last (read heapIn) of
      TSOHead _ hd a -> do
        forwardedNode .= fmap FetchMe hd -- Restore an old fetchme at the head node
        heapOut .:= repeat RamNoOp ++ singleton (RamRead a)
        -- If this resume happens during a root ID phase, let the GC know in case it has been missed!
        upd .:= Just (Enter a $ TSOHead Nothing hd a)
        top' .= Ptr PShared a -- Set top' to keep prefetching going when if we get blocked
      TSOPar rest n as -> do
        let offset = resize n :: Len (2^(1+Log2MaxPush)-1)
        -- Adjust offset to we overwrite the original startAddress on first stack resume cycle
        first <- (1==) <$> use stalls
        stalls .= 0
        when (first && size vStkIn /= 1) $
          failure ErrMutResumeFormat
        let offset' = if first then pred (bitCoerce offset) else bitCoerce offset
        updateV offset' (as ++ repeat Nothing)
        case rest of
          Nothing -> finish (fromJust $ head as)
          Just a  -> heapOut .:= repeat RamNoOp ++ singleton (RamRead a) >>
                     top' .= Ptr PShared a
      TSORest rest p c u -> do
        maybe (pure ()) pushP p
        maybe (pure ()) pushA c
        maybe (pure ()) pushU u
        case rest of
          Nothing -> finish (top vStkIn)
          Just a  -> heapOut .:= repeat RamNoOp ++ singleton (RamRead a) >>
                     top' .= Ptr PShared a
      _ -> failure ErrMutResumeNode
  where
    finish t = do
      mutThreadCmd .:= Just PopReady
      -- ^ NB We only pop the TSO from the ready list once we've restored its state.
      -- Otherwise we have awkward race conditions with GC.
      -- TODO surely this needs gating on ready?
      phase .= Reduce
      allocBubble .:= False
      top' .= t

-- Unwind a heap application onto the stack
unwind :: Atom -> Pure CPUIn -> CPU ()
-- Special case for `par` subjects
unwind (Ptr PPar haddr) CPUIn{..} = do
  updateV (-1) (repeat Nothing)
  forwardedNode .= Nothing
  top' .= at d1 (read vStkIn)
  mutThreadCmd .:= Just (PushSpark haddr)
unwind (Ptr tag  haddr) CPUIn{..} = do
  -- Node to unwind is either forwarded via state or prefetched from heap.
  fwd <- use forwardedNode
  alreadyLocked <- use forceEntry
  let node = fromMaybe (last $ read heapIn) fwd
  forwardedNode .= Nothing
  forceEntry    .= False

  case node of
    Locked bs -> do
      -- At this point, we know that we need to suspend but we need the
      -- scheduler to put us on the blocking list. So we jump to Fetch handler instead.
      tso <- use tsoAddr
      heapOut      .:= RamWrite haddr (Locked $ Just (coreId,tso)) :> repeat RamNoOp
      upd          .:= Just (Modify haddr)
      allocBubble  .:= False
      schedRequest .:= Just (MCLocalSuspend tso bs)
      forwardedNode .= Just node
      phase         .= Fetch
      fetching      .= FOWait coreId
    FetchMe ga -> goFetch (isJust fwd) ga
    TSOHead {} -> fail
    TSOPar  {} -> fail
    TSORest {} -> fail
    _          -> go alreadyLocked node
  where
    fail = failure ErrMutUnwindTSO

    goFetch locked src@(cid, _) = do
      -- Lock node until we get a response (unless already locked)
      unless locked $ do
        heapOut .:= RamWrite haddr (Locked Nothing) :> repeat RamNoOp
        upd     .:= Just (Enter haddr $ FetchMe src)
      tso <- use tsoAddr
      phase    .= Fetch
      fetching .= FOWait cid
      forwardedNode .= Just (FetchMe src)
      schedRequest .:= Just (MCFetch tso src haddr)

    go locked node =  do
      let shared = isTagShared tag
      let doSeq  = tag == PSeq
      let unode  = unpackNode shared node :: UnpackedNode CMaxPush
      let offset = (unpack . resize . pack $ nArity unode) - 1
      let as     = map (fmap (dashIf shared)) (nAtoms unode)
      let rdAddr = head (nAtoms unode) >>= heapAddr

      -- Possibly register update address and lock
      when (nUpdatable unode) $ do
        mutThreadCmd .:= Just (FizzleSpark haddr)
        let wrOp = if locked then RamNoOp else RamWrite haddr (Locked Nothing)
        heapOut .:= wrOp                            :>
                    maybe RamNoOp RamRead rdAddr    :>
                    repeat RamNoOp
        upd     .:= Just (Enter haddr node)
        pushU $ Update { uAddr    = Just haddr
                       , uSp      = size vStkIn
                       , uDiscard = doSeq
                       , uRoot    = False
                       }

      -- Possibly register case table
      forM_ (nCaseTable unode) pushA

      -- TODO Reinstate immediate deallocations, while respecting spark pool membership.
      -- TODO Be super careful about PAR subjects here. We no longer count them as shared. If we free them, there might be consequences.
      -- Free any non-shared unwound apps
      -- unless shared
      --        (gcRequest .:= RDealloc haddr)

      -- Update main stack
      if doSeq && not (nUpdatable unode)
        then updateV (-1) (repeat Nothing) >>
             top' .= at d1 (read vStkIn) -- Ignore SEQs for non-updatable apps
        else updateV offset as           -- Unwind onto stack
unwind _ _ = failure ErrMutUnwindNonPtr

-- Perform primitive operations
prim :: Pure CPUIn -> CPU ()
prim CPUIn{..} =
  case primOpPat (map Just (read vStkIn)) of
    -- Args already evaled; perform op
    BothInt x (_, swp, op) y ->
      updateV (-2) (Just (doOp op swp x y) :> repeat Nothing)

    -- Both args now evaluated but first is on prim stack; perform op
    SndInt y (_, swp, op) ->
      case _top pStkIn of
        Nothing -> failure ErrMutNotRedex
        Just x  -> popP >>
                   updateV (-1) (Just (doOp op swp x y) :> repeat Nothing)

    -- Second arg unevaluated; push first onto prim stack
    FstInt x y ->
      pushP x >>
      updateV (-1) (Just y :> repeat Nothing)

    -- Special case for unwrapping (used in worker/wrapper optimisation)
    Unwrap x f ->
      updateV (-1) (Just f :> Just (PrimInt x) :> repeat Nothing)

    -- Unexpected prim op pattern
    NotPrim -> failure ErrMutNotRedex
  where
    doOp op swp a b = alu $ AluIn op swp a b

-- Update a heap node with its normal form
update :: Pure CPUIn -> CPU ()
update CPUIn{..} = case read uStkIn of
  Nothing ->
    error "Core.Core.update: Read from empty update stack"
  Just u -> do

    --Dash the NF application on the stack, marking it as possibly shared
    let stkAs   = read vStkIn
    let nfArity = atomArity (head stkAs)
    let stkAs'  = imap (\i a -> if i < resize nfArity
                                  then Just (dash a)
                                  else Nothing) stkAs
    let finishTask = size uStkIn <= 1
    let finishProg = uRoot . fromJust $ read uStkIn
    popU

    if uDiscard u || (finishTask && not finishProg)
      then let offset = resize . bitCoerce $ 1 + size vStkIn - uSp u
               t = read vStkIn !! offset
           in do updateV (negate offset) (repeat Nothing)
                 top' .= t
                 when (isJust (heapAddr t)) (stalls .= 0 >> phase .= Stall)
              -- ^ If we're discarding this update, we might force a new heap pointer onto the top of stack.
              -- This needs prefetching, but we don't have any ports free...
      else updateV 0 stkAs'

    -- If this is the end of a task, jump to the right phase
    when finishTask (phase .= Init >> tsoAddr .= 0)
    when finishProg (phase .= Halt)

    case uAddr u of
      -- TODO I don't think GC does update invalidation anymore? Can we remove this?
      Nothing -> pure () -- This update was invalidated by the GC --- don't commit to heap.
      Just ua -> do -- Commit to heap
        upd .:= Just (Release ua)

        -- Write the normal form to heap
        if nfArity <= nodeLen

          -- Small normal form: Fits in one heap node
          then do
            let n = App True (resize nfArity) (takeI stkAs')
            specialiseHeap heapConfig
              -- UltraRAM
              (heapOut .:= RamRead ua :> RamWrite ua n :> repeat RamNoOp)
              -- BlockRAM
              (heapOut .:= RamWrite ua n               :> repeat RamNoOp)

          -- Large normal form: Split into two heap nodes
          -- e.g. NF [a,b,c,d,e,f] -> x |-> [a,b,c,d]; y |-> [@x, e, f]
          -- We don't need to worry about the bubble signal here. Normal forms
          -- are either function apps, constructor apps, prim op apps, or integers.
          -- Function and constructor cases are already handled.
          -- Prim ops and ints are never wide enough to merit an allocation during update.
          else do
            let n1 = App True (resize nodeLen) (takeI stkAs')
                n1Addr = head nextAddrs
                n2Args = takeI $ dropI @NodeLen stkAs' ++
                                 repeat @NodeLen Nothing
                n2 = App True (resize $ nfArity + 1 - nodeLen)
                         (Just (Ptr PShared n1Addr) :> n2Args)
            gcRequest .:= RAlloc (True :> repeat False)
            specialiseHeap heapConfig
              -- UltraRAM
              (wideUpd .= (n1Addr, n1, finishProg)   >>
               phase   .= ContinueUpdate >>
               heapOut .:= RamRead ua :> RamWrite ua n2 :> repeat RamNoOp)
              -- BlockRAM
              (heapOut .:= RamWrite ua n2 :> RamWrite n1Addr n1 :> repeat RamNoOp)
  where
    nodeLen = snatToNum (SNat @NodeLen)

-- Instantiate a template
unfold :: Pure CPUIn -> CPU ()
unfold CPUIn{..} =
  do -- If we came from a case expression, pop from alternative stack
     let t = head $ read vStkIn
     when (isCon t) popA

     -- Freeze args when starting new chain of split templates
     when (newTmplChain t)
          (frozenArgs .= takeI (read vStkIn))
     args <- use frozenArgs

     -- Instantiate all template atoms
     -- Resolves ARGs, REGs, and PTRs
     primRegs <- use regs
     prevAddrs <- use allocs
     let curAddrs = nextAddrs
     let Template pushOffset spineAp heapAps =
           mapTemplate (inst prevAddrs curAddrs args primRegs)
                       tmplIn

     -- Push spine ap to stacks
     let uSpineAp = unpackNode False spineAp :: UnpackedNode CMaxPush
     updateV pushOffset (nAtoms uSpineAp)
     when (nAtoms uSpineAp == repeat Nothing)
          (top' .= at d1 (read vStkIn))

     maybe (return ()) pushA (nCaseTable uSpineAp)

     -- Heap prefetching logic, using forwarding for references to newly
     -- allocated nodes
     let atoms :: Vec MaxPush (Maybe Atom)  = nAtoms $ unpackNode @MaxPush False $ tSpine tmplIn
     let headPtr = head atoms >>= heapAddr
     let defReadCtrl = maybe RamNoOp RamRead $
                       head (nAtoms uSpineAp) <|> Just (at d1 (read vStkIn)) >>= heapAddr
     let (readCtrl, fwdNode)
           = case headPtr of
               Just addr -> (if addr < fromSNat (lengthS heapAps) then (RamNoOp, heapAps !! addr) else (defReadCtrl, Nothing))
               Nothing   -> (defReadCtrl, Nothing)
     forwardedNode .= fwdNode

     -- Allocate heap aps
     let (regWrs, hWrs) = unzip $ zipWith allocAp curAddrs heapAps
     -- TODO could model regs as a tiny TDP LUTRAM with async read?
     regs .= foldr updReg primRegs regWrs
     -- let heapWrs = foldl shiftHeapOps (repeat Nothing) (reverse hWrs)
     prevAllocs <- use allocs
     allocs .= foldl shiftAllocBuf prevAllocs hWrs
     arbitrate readCtrl hWrs
     gcRequest .:= RAlloc (map wr hWrs)
  where
    newTmplChain (Fun _ _ False) = False
    newTmplChain _               = True
    isReducible (BothInt x (_, _, op) y) = Just (x,op,y)
    isReducible _                        = Nothing

    allocAp addr (Just (Prim reg _ as)) =
      case isReducible (primOpPat as) of
        -- If arguments are ints, reduce op now; no heap allocation
        Just (x, op, y) -> (Just (reg, alu $ AluIn op False x y), RamRead 0)
        -- ^ Above RamRead is to ensure that mutator always keeps portA busy while other writes are dispatched.
        --   Issues would appear in tak.fl where a function has two PRS apps and only the second is successful.
        -- Otherwise, dump onto heap
        Nothing -> (Just (reg, Ptr PUniq addr), RamWrite addr (App False 3 (as ++ repeat Nothing)))
    allocAp addr (Just app) = (Nothing, RamWrite addr app)
    allocAp _ Nothing = (Nothing, RamNoOp)

    updReg (Just (i, n)) = replace i n
    updReg Nothing       = id

    shiftAllocBuf ops (RamWrite i _) = i +>> ops
    shiftAllocBuf ops _              = ops

    isNoOp RamNoOp = True
    isNoOp _       = False

    -- If we can't prefetch from the heap, we might need to stall
    arbitrate rd wrs = do
      let (opDo, opSkip) = orderRamOp (last wrs) rd
      let ops = init wrs ++ singleton opDo
      heapOut .:= ops
      phase .= if isNoOp opSkip then Reduce else Stall

    orderRamOp RamNoOp  b = (b, RamNoOp)
    orderRamOp a        b = (a, b)

    wr (RamWrite _ _) = True
    wr _              = False

-- Instantiate simple, non-function valued inline case alternatives
caseSelect :: Pure CPUIn -> CPU ()
caseSelect CPUIn{..} =
  do let args = takeI @CMaxPush $ read vStkIn
     let ct   = _top aStkIn
     t <- use top'
     popA

     -- Select correct alternative
     case getAlt (t, ct) of
       Just (n, x, y) ->
         do let alt = if bitToBool (lsb n) then y else x

            -- Instantiate alt on vstack
            (offset, atom) <- instAlt args alt
            updateV offset (Just atom :> repeat Nothing)
       Nothing -> failure ErrMutNotRedex
  where
    getAlt (Con _ n, Just (CTInline x y)) = Just (n, x, y)
    getAlt _                              = Nothing

dumpRoots :: Pure CPUIn -> CPU ()
dumpRoots CPUIn{..} =
  do gcRequest .:= RRoot (read vStkIn)
     retPhase <- use prevPhase
     when (gcCmd /= RootsCmd)
          (allocBubble .:= False >> phase .= retPhase)

continueUpdate :: Pure CPUIn -> CPU ()
continueUpdate CPUIn{..} = do
  let finishTask = size uStkIn == 0
  (addr, node, finishProg) <- use wideUpd
  rd <- heapAddr <$> use top'
  -- If we discarded the update's WHNF (for a seq) we might need to prefetch the top of stack too.
  heapOut .:= RamWrite addr node :> maybe RamNoOp RamRead rd :> repeat RamNoOp
  phase .= Reduce
  when finishTask (phase .= Init >> tsoAddr .= 0)
  when finishProg (phase .= Halt)

stall :: CPU ()
stall = use stalls >>= \case
  0 -> phase  .= Reduce
  n -> stalls .= n-1

-- Helper functions

needsUnwind :: Atom -> Bool
needsUnwind (Ptr {}) = True
needsUnwind _        = False

needsUpdate :: Atom -> Pure CPUIn -> Bool
needsUpdate t CPUIn{..}
  = maybe False go (read uStkIn)
  where
    go u
      = resize (atomArity t) > satSub SatBound (size vStkIn) (uSp u)

needsUnfold :: Atom -> Pure CPUIn -> Bool
needsUnfold t CPUIn{..} = go t (_top aStkIn)
  where
    go (Fun {}) _ = True
    go (Con _ _) (Just (CTOffset _)) = True
    go (Con _ tag) (Just (CTInline (UAFun _) _))
      | lsb tag == low  = True
    go (Con _ tag) (Just (CTInline _ (UAFun _)))
      | lsb tag == high = True
    go _ _ = False

-- Instantiate an atom relative to current heap pointer, top spine values, and
-- register contents
inst :: Vec MaxApSpan HeapAddr -> Vec MaxAps HeapAddr -> Vec CMaxPush Atom -> Vec MaxRegs Atom ->
        Atom -> Atom
inst _ _ spine _ (Arg tag indx)
  = let arg = spine !! (1 + resize indx :: Index CMaxPush)
    in case tag of
         PShared -> dash arg
         PUniq   -> arg
         t       -> forcePtrTag t arg
inst prevAddrs curAddrs _ _ (Ptr tag addr)
  | addr < snatToNum (SNat @MaxAps) = Ptr tag $ curAddrs !! addr
  | otherwise -- We need to look back at recent allocations
  = Ptr tag $ prevAddrs !! (negate addr - 1)
inst _ _ _ regsv (Reg shared indx)
  = dashIf shared $ regsv !! indx
inst _ _ _ _ a = a

-- Instantiate an atom relative to current heap pointer, top spine values, and
-- register contents
instAlt :: Vec CMaxPush Atom -> UnpackedAlt -> CPU (Offset Log2MaxPush, Atom)
instAlt _    (UAFun _) = failure ErrMutSimpleAltFn >> pure (0,unpack 0)
instAlt _    (UAInt pops val      ) = pure (altPushOffset pops, PrimInt   (resize val))
instAlt _    (UACon pops arity tag) = pure (altPushOffset pops, Con arity (resize tag))
instAlt args (UAArg pops idx      ) = pure (altPushOffset pops, args !! i)
  where i = 1 + resize idx :: Index CMaxPush

pushA :: CaseTable UnpackedAlt -> CPU ()
pushA tab = aStkPush .:= Just tab

popA :: CPU ()
popA = aStkPop .:= True

pushU :: Update -> CPU ()
pushU u = uStkPush .:= Just u

popU :: CPU ()
popU = uStkPop .:= True

pushP :: PInt -> CPU ()
pushP p = pStkPush .:= Just p

popP :: CPU ()
popP = pStkPop .:= True

updateV :: Offset Log2MaxPush -> Vec CMaxPush (Maybe Atom) -> CPU ()
updateV off as =
  vStkOut .:= Just (off, as ++ repeat Nothing) >>
  forM_ (head as) (top' .=)
  -- Update top' state to inform memory prefetching in `defaultOutput`

switchPhase :: Phase -> Phase -> CPU ()
switchPhase p q =
  prevPhase .= p >>
  phase     .= q

-- We need to ensure we don't alloc bubble when entering Suspend phase.
beginSuspend :: CPU ()
beginSuspend = allocBubble .:= False >> phase .= Suspend

latchMaxStall :: CPU ()
latchMaxStall = do
  n <- use curStall
  stats . maxStall %= max n

-- TODO Infer a stat increment based on the end phase in defaultOutput? That'd be much cooler.
