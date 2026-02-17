{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-} -- For unused generated lenses
{-| The Cloaca garbage collector
-}
module Heron.Core.Collector
  ( GCCmd(..)
  , GCBarrier(..)
  , GCRequest(..)
  , GCNode(..)
  , GCIn(..)
  , GCState(..)
  , Phase(..)
  , GCOut(..)
  , GCList(..)
  , initGCState
  , tailPtr
  , gc
  , runGc
  ) where

import           Barbies.TH
import           Clash.Prelude        hiding (read)
import           Control.Arrow
import           Control.DeepSeq
import           Control.Lens         hiding (Index, assign, at, both, imap, op,
                                       (:>))
import           Control.Monad
import           Control.Monad.State
import           Data.Either
import           Data.Maybe           (fromJust, fromMaybe, isJust, isNothing)
import           RetroClash.Barbies
import           RetroClash.CPU       hiding (update)

import           Heron.Core.Fifo
import           Heron.Core.Heap
import           Heron.Core.ParStack  (PSAddr)
import           Heron.Core.Types
import           Heron.Error
import           Heron.Parameters
import           Heron.Schedule.Types (RefCountCmd (..), RefCountNode (..),
                                       ThreadCmd (..), ThreadResult (..),
                                       ThreadTag (..))
import           Heron.Template
import           Heron.TraceFSM

type Deeply = Bool

-- | Identifies the kind of GC metadata for a heap address.
data GCNode
  = Unmarked
  -- ^ Possibly unreachable data (not _yet_ marked)
  | Marked Deeply
  -- ^ Definitely reachable data
  | FreeList HeapAddr
  -- ^ Definitely unreachable data (part of the `FreeList` linked-list)
  | WorkQueue HeapAddr
  -- ^ Definitely reachable data whose subgraphs are yet to be marked (part of
  -- the `WorkQueue` linked-list)
  deriving (Generic, NFDataX, Show, ShowX, Eq, NFData, BitPack)

isMarked :: GCNode -> Bool
isMarked (Marked _) = True
isMarked _          = False

{- TODO Maybe there is better performance to be gained if we refactor the various
 `pickOp` helpers into state calculated from the previous cycle, where possible.
-}

-- | Get the pointer to the linked-list tail from a `GCNode`
tailPtr :: GCNode -> HeapAddr
tailPtr Unmarked      = errorX "Unmarked GC nodes are not part of a linked-list"
tailPtr (Marked _)    = errorX "Marked GC nodes are not part of a linked-list"
tailPtr (FreeList a)  = a
tailPtr (WorkQueue a) = a

-- | Commands passed to the mutator
data GCCmd
  = NoCmd
  -- ^ Keep calm and carry on
  | RootsCmd
  -- ^ Once in a safe state, dump the graph roots from the value stack
  | WaitBufCmd
  -- ^ Mutation buffer is full up --- pause please.
  | WaitCmd
  -- ^ Heap is full up --- pause please.
  deriving (Generic, NFDataX, Show, ShowX, Eq, BitPack)

data GCBarrier
  = RootBarrierCmd
  -- ^ GC is performing TSO root ID now, so we need to prevent mutator suspends/resumes
  | UpdateBarrierCmd
  -- ^ GC is performing marking now, so it needs to know about any updates
  -- committed to the heap. This also prevents resume/suspend operations
  deriving (Generic, NFDataX, Show, ShowX, Eq, BitPack)

-- | Collector phase
data Phase
  = Init HeapAddr
  | Idle
  | RootsStack
  | RootsUStk (Index UStkSize)
  | RootsBlocked
  | RootsReady
  | RootsRef
  | Mark
  | SweepSparks
  | SweepRefs
  | SweepHeap (Maybe HeapAddr)
  | Halt
  deriving (Generic, NFDataX, Show, ShowX, Eq, BitPack)

-- | Linked list metadata
data GCList = GCList
  { _llLen    :: HeapAddr
  , _llHead   :: HeapAddr
  , _llUpdate :: Bool
  }
  deriving (Generic, NFDataX, Show, ShowX, Eq, BitPack)
makeLenses ''GCList

-- TODO Should make helpers for pop and push. What's the general pattern?

-- | An incoming request from the mutator
data GCRequest
  = RAlloc (Vec MaxAps Bool)
    -- ^ Addresses which have been allocated by the mutator
  | RDealloc HeapAddr
    -- ^ Address which can be immediately deallocated
  | RRoot (Vec CMaxPush Atom)
    -- ^ Graph roots passed from the mutator
  | RFinished
  | RNothing
    -- ^ Signals that the reduction has completed and we are free to reset the
    -- bookkeeping memory
  deriving (Generic, NFDataX, Show, ShowX, Eq, BitPack)

type StackState = (PSAddr VStkSize, Atom)
  -- ^ (Current stack size, Stack element read from the snoop port)

-- | Inputs to the GC subsystem
declareBareB [d|
  data GCIn = GCIn
    { gcMemIn   :: HeapOut GCNode MaxAps HeapSize
    -- ^ Words read from the bookkeeping memory
    , heapMemIn :: Maybe HeapNode
    -- ^ Word read from the shared heap memory
    , bubble    :: Bool
    -- ^ A flag suggesting when the mutator has a gap between its allocations.
    -- Since the collector sees the mutator outputs one cycle later, this gives
    -- us an indication of what the mutator is doing _now_.
    , mutUpdateIn  :: FOut HeapNode GCMutBufSize
    , comUpdateIn  :: FOut HeapNode GCMutBufSize
    -- ^ The previous contents of any heap application which has been
    -- overwritten during marking
    , triggerThres :: HeapAddr
    -- ^ How small does any freelist need to be before we trigger a GC pass?
    , request  :: GCRequest
    -- ^ Requests sent from the mutator
    , stkIn :: StackState
    -- ^ Stack state details for root ID
    , rcReady :: Bool
    , rcNextRef :: Maybe (Maybe (HeapAddr, RefCountNode))
    , threadReady :: Bool
    , threadRet :: Maybe ThreadResult
    , curTSO :: HeapAddr
    , mutSuspending :: Bool
    , ustkIn :: Update
    , ustkRet :: Bool
    , ustkWarn :: Bool
    } |]
deriving instance Generic (Pure GCIn)
deriving instance NFDataX (Pure GCIn)
deriving instance BitPack (Pure GCIn)
deriving instance Show    (Pure GCIn)
deriving instance ShowX   (Pure GCIn)

type FreeLists = Vec MaxAps (GCList, GCList)

-- | The collector's internal state
data GCState = GCState
  { _phase     :: Phase
  , _postMark  :: Maybe Phase
  , _freelist  :: FreeLists
  -- ^ We maintain MaxAps pairs of freelists. Each channel needs two freelists
  -- because of the latency between mutator and collector. As soon as the
  -- mutator performs an allocation, it can swap to using the second list in the
  -- pair while the collector returns the first element to a valid state.
  , _worklist  :: GCList
  , _heapLatch :: Either HeapAddr (Len NodeLen, Vec NodeLen (Markable Atom))
  -- ^ A latched copy of the latest application read from the shared heap
  -- memory, along with an index to the current atom for marking
  , _workLatch :: Maybe GCNode
  -- ^ A latched copy of the latest node read from the bookkeeping memory's workqueue.
  , _prevAddr  :: Maybe HeapAddr
  -- ^ The previous address "used" --- has different purposes for each GC phase.
  , _unproductivePasses  :: Unsigned 4
  -- ^ Statistics for detecting heap exhaustion. We might need multiple GC
  -- passes before we're sure the heap is full of live data because anything
  -- allocated during marking is assumed to be live.
  , _mutBufSz  :: Unsigned (CLog 2 GCMutBufSize)
  -- ^ The current size of the mutator's update buffer
  , _rootSp :: PSAddr VStkSize
  , _sp :: PSAddr VStkSize
  , _ustkLatch :: Maybe Update
  , _ustkOpLatch :: RamOp UStkSize Update
  , _ustkEnd :: Bool
  , _descend :: Bool -- When marking this element, should we descend into its
                     -- subgraphs or just mark it and move on (for reserved
                     -- addresses that don't count as roots)
  }
  deriving (Generic, NFDataX, Show, ShowX, BitPack, AutoReg)
makeLenses ''GCState

-- | Outputs from the collector subsystem
declareBareB [d|
  data GCOut = GCOut
    { _gcMemOut   :: HeapIn GCNode MaxAps HeapSize
    -- ^ Operations for the bookkeeping memory
    , _heapMemOut :: Maybe HeapAddr
    -- ^ An operation for the shared heap memory. The mutator always gets
    -- priority, so this might take a few cycles.
    , _nextFree   :: Vec MaxAps (Maybe HeapAddr, Maybe HeapAddr)
    -- ^ The next few free addresses, ready for allocation.
    , _cmd        :: GCCmd
    -- ^ The command to be passed to the mutator
    , _barrier    :: Maybe GCBarrier
    -- ^ The barrier to be
    , _comUpdatePop  :: Bool
    , _mutUpdatePop  :: Bool
    -- ^ Pop an element from the mutation update buffer.
    , _remaining  :: Vec MaxAps (HeapAddr, HeapAddr)
    -- -- ^ The number of remaining free addresses on each channel of freelists.
    -- -- For debugging only.
    , _stkSnoopAddr :: PSAddr VStkSize
    , _rcCmd :: Maybe RefCountCmd
    , _rcRetPop :: Bool
    , _threadCmd :: Maybe ThreadCmd
    , _threadRetPop :: Bool
    , _ustkSnoop :: RamOp UStkSize Update
    , _errGc :: Maybe Err
    } |]
makeLenses ''GCOut
deriving instance Generic (Pure GCOut)
deriving instance NFDataX (Pure GCOut)
deriving instance BitPack (Pure GCOut)
deriving instance Show    (Pure GCOut)
deriving instance ShowX   (Pure GCOut)

-- | Initial GC state
initGCState :: GCState
initGCState = GCState
  { _phase = Init 0
  , _postMark = Nothing
  , _freelist = fl
  , _worklist = wl
  , _heapLatch = Right (0, repeat Nothing)
  , _workLatch = Just $ WorkQueue 0
  , _prevAddr  = Nothing
  , _unproductivePasses  = 0
  , _mutBufSz  = 0
  , _rootSp = 0
  , _sp = 0
  , _ustkEnd = False
  , _ustkLatch = Nothing
  , _ustkOpLatch = RamNoOp
  , _descend = True
  }
  where
    fl = (GCList quartLength  0           False
         ,GCList quartLength  quartLength False
         ) :>
         (GCList quartLength  (2*quartLength) False
         ,GCList quartLength  (3*quartLength) False
         ) :>
         Nil
    wl = GCList 0 0 False
    quartLength = snatToNum (SNat @(HeapSize `Div` 4))

heapFull :: FreeLists -> Bool
heapFull = fold (||) . map (uncurry (||) . both ((minAddrs >=) . _llLen))
  where
    minAddrs = snatToNum $ SNat @(MaxFnAps `Div` 2)

nearStackPtr :: PSAddr VStkSize -> PSAddr VStkSize -> Bool
nearStackPtr sPtr rPtr = rPtr + snatToNum (SNat @(CMaxPush + CMaxPush)) >= sPtr

flBundle :: Show a => Vec 2 (a,a) -> Vec 4 a
flBundle ((a,b) :> (c,d) :> Nil) = a :> b :> c :> d :> Nil
flBundle x = errorX $ unwords
  [ "Heron.Core.Collector.flBundle: Unexpected freelists structure "
  , show x
  ]

flUnbundle :: Show a => Vec 4 a -> Vec 2 (a,a)
flUnbundle (a :> b :> c :> d :> Nil) = (a,b) :> (c,d) :> Nil
flUnbundle x = errorX $ unwords
  [ "Heron.Core.Collector.flBundle: Unexpected unbundled freelists structure "
  , show x
  ]

onSmallestList :: (GCList -> (a, GCList)) -> Bool -> GC a
onSmallestList f bubble = do
  (a, ls) <- if bubble
               then onSmallest'    f <$> use freelist
               else onSmallestSnd' f <$> use freelist
  freelist .= ls
  pure a

onSmallest' :: (GCList -> (a, GCList)) -> FreeLists -> (a, FreeLists)
onSmallest' f ls = (acc, flUnbundle $ replace n x ls')
  where
    ls' = flBundle ls
    (acc, x) = f l
    (n, l) = fold pickList $ imap (,) ls'
    pickList (i,a) (j,b)
      | _llLen a > _llLen b = (j,b)
      | otherwise           = (i,a)

onSmallestSnd' :: (GCList -> (a, GCList)) -> FreeLists -> (a, FreeLists)
onSmallestSnd' f ls = (acc, replace n (x,y) ls)
  where
    (x, (acc, y)) = second f l
    (n, l) = fold pickList $ imap (,) ls
    pickList (i,a) (j,b)
      | la > lb   = (j,b)
      | otherwise = (i,a)
      where la = pairLen a
            lb = pairLen b

push :: HeapAddr -> GCList -> (RamOp HeapSize GCNode, GCList)
push n l =
  ( RamWrite n . FreeList $ _llHead l
  , l & llLen  %~ (+ 1)
      & llHead .~ n
  )

defaultOutput :: GCState -> Pure GCOut
defaultOutput GCState{..} = GCOut
  { _gcMemOut = repeat RamNoOp
  , _heapMemOut = either Just (const Nothing) _heapLatch
  , _nextFree = map (both nextHead) _freelist
  , _cmd      = cmd'
  , _barrier  = barrier'
  , _remaining = map (both _llLen) _freelist
  , _comUpdatePop = False
  , _mutUpdatePop = False
  , _stkSnoopAddr = _rootSp
  , _rcCmd = Nothing
  , _rcRetPop = False
  , _threadCmd = Nothing
  , _threadRetPop = False
  , _ustkSnoop = _ustkOpLatch
  , _errGc = if _phase == Halt then Just ErrHeapFull else Nothing
  }
  where
    mutBufFull = _mutBufSz >= snatToNum (SNat @(GCMutBufSize-2))
    cmd' = fromMaybe NoCmd $ needsRoot <|> needsWait
    barrier' = needsRootBarrier <|> needsUpdBarrier
    needsRoot
      | _phase == RootsStack && nearStackPtr _sp _rootSp = Just RootsCmd
        -- ^ We (ab)use the worklist's update flag to signal when the mutator need to be paused for sequential root ID.
      | otherwise = Nothing
    needsWait
      | isInit _phase
        || heapFull _freelist = Just WaitCmd
      | mutBufFull            = Just WaitBufCmd
      | otherwise = Nothing
    needsRootBarrier
      | _phase == RootsStack   = Just RootBarrierCmd
      | isUStk _phase = Just RootBarrierCmd
      | _phase == RootsBlocked = Just RootBarrierCmd
      | _phase == RootsReady   = Just RootBarrierCmd
      | maybe False isUStk _postMark = Just RootBarrierCmd
      | _postMark == Just RootsBlocked = Just RootBarrierCmd
      | _postMark == Just RootsReady   = Just RootBarrierCmd
      | otherwise = Nothing
    needsUpdBarrier
      | _phase == Mark     = Just UpdateBarrierCmd
      | _phase == RootsRef = Just UpdateBarrierCmd
      | _phase == SweepSparks = Just UpdateBarrierCmd
      | otherwise = Nothing

    nextHead l
      | _llUpdate l = Nothing -- Freelist needs one cycle to return to stable state
      | otherwise   = Just $ _llHead l
    isInit (Init _) = True
    isInit _        = False
    isUStk (RootsUStk _) = True
    isUStk _             = False

-- | Synchronous control logic, packaged as a Mealy machine
gc :: (HiddenClockResetEnable dom)
    => Signal dom CoreId -> Signals dom GCIn -> Signals dom GCOut
gc = traceFSM "_gc" initGCState defaultOutput step
{-# NOINLINE gc #-}

-- | Simulate the collector for one cycle
runGc :: Pure GCIn -> State GCState (Pure GCOut)
runGc i = runCPU defaultOutput $ step i

type GC = CPUM GCState GCOut

-- | Dispatch for GC phases
step :: Pure GCIn -> GC ()
step GCIn{..} = do
  -- Resolve new heads for any freelists which have been popped from during the
  -- previous cycle
  updateNexts gcMemIn
  -- Latch any reads from GC bookkeeping and shared heap memories
  latchReads heapMemIn gcMemIn
  -- Latch any communications to ustk
  updateUStk ustkIn ustkRet ustkWarn
  -- Register some inputs as state for `defaultOutput`
  mutBufSz .= max (size comUpdateIn) (size mutUpdateIn)
  sp .= fst stkIn
  -- Go
  p <- use phase
  case safeReq p request of
    RAlloc as  -> alloc as bubble
    RDealloc a -> free a bubble
    RFinished  -> phase .= Init 0 >>
                  put initGCState
    _          -> gcTasks p
  where
  safeReq _ (RAlloc as)
    | fold (||) as = RAlloc as
  safeReq p (RDealloc a)
    | deallocSafe p a = RDealloc a
  safeReq _ RFinished = RFinished
  safeReq _ _         = RNothing

  gcTasks (Init n) = initFreelists n
  gcTasks Idle     = do
    fls <- use freelist
    if gcTrigger fls
      then do
        when bubble (void balanceFreelists)
        when (heapFull fls) (unproductivePasses %= (+1))
        phase .= RootsStack
      else when bubble (void balanceFreelists >> redistribute)
  gcTasks RootsStack = when bubble (void balanceFreelists) >>
                       gatherRoots (rootsMsg request) stkIn curTSO
  gcTasks (RootsUStk x) = gatherRootsUStk x
  gcTasks RootsBlocked = do fls <- use freelist
                            when bubble (void balanceFreelists)
                            unless (mutSuspending && not (heapFull fls)) $
                              gatherRootsBlocked threadReady threadRet
  gcTasks RootsReady   = when bubble (void balanceFreelists) >>
                         gatherRootsReady threadReady threadRet
  gcTasks RootsRef     = when bubble (void balanceFreelists) >>
                         gatherRootsRef rcReady rcNextRef
  gcTasks Mark     = do
    when bubble (void balanceFreelists)
    blocked <- isLeft <$> use heapLatch
    unless blocked (mark mutUpdateIn comUpdateIn)
  gcTasks SweepSparks   = sweepSparks threadReady threadRet
  gcTasks SweepRefs     = sweepRefs rcReady rcNextRef
  gcTasks (SweepHeap x) = sweep x bubble
  gcTasks Halt      = pure ()
  -- TODO There's some scope for lifting on the balanceFreelists call into one
  -- shared place. Does the replication as it stands work better than any extra
  -- routing constraints when shared?

  gcTrigger = fold (||) . map (uncurry (||) . both ((triggerThres >) . _llLen))
  rootsMsg (RRoot tops) = Just tops
  rootsMsg _            = Nothing

-- | Perform sweeping of the marked address space
sweep :: Maybe HeapAddr -> Bool -> GC ()
sweep x bubble =
  case x of
    -- Finished sweeping
    Nothing -> do
      write    <- writeOp
      gcMemOut .:= RamNoOp :> write :> repeat RamNoOp
      prevAddr .= Nothing
      phase .= Idle
      p <- use unproductivePasses
      if p==maxBound
        then phase .= Idle -- Halt TODO Removed to test for space leaks. Bad examples wont terminate; slow examples will
        else phase .= Idle

    -- Continue sweeping
    Just addr -> do
      wr <- writeOp
      prevAddr .= Just addr
      gcMemOut .:= RamRead addr :> wr :> repeat RamNoOp
      workLatch .= Nothing
      phase .= if addr == maxBound
                 then SweepHeap Nothing
                 else SweepHeap (Just $ addr+1)
  where
    -- Inspect any previous read and derive write operation
    writeOp = do
      pa <- use prevAddr
      wl <- use workLatch
      case liftA2 (,) pa wl of
        Nothing -> return RamNoOp
        Just (a, Marked _)      -> return $ RamWrite a Unmarked
        Just (_, FreeList  _) -> return RamNoOp
        Just (_, WorkQueue _) -> failure ErrGcWorkDuringSweep >> return RamNoOp
        Just (a, Unmarked) -> do
            unproductivePasses .= 0
            onSmallestList (push a) bubble

failure :: Err -> GC ()
failure e = errGc .:= Just e

-- Control structure to direct the marking phase
data MarkOp
  = MarkChild
  -- ^ Handle the next child atom of our current application.
  -- Read its `GCNode`. Next cycle it will be added to the work queue iff it is
  -- not already there.
  | MarkFetchBuffer
  -- ^ Mark and fetch a new application from the mutator's mutation buffer.
  -- These are prioritised for marking over fetching from the heap since the
  -- buffer is small FIFO.
  | MarkFetchQueue
  -- ^ Mark and fetch a new application from the work queue.
  | MarkFinish
  -- ^ We're done and ready for sweeping
  deriving (Generic, NFDataX, Show, ShowX, Eq)

-- | Incrementally append any unvisited children to the workqueue, marking each address as we go
mark :: FOut HeapNode GCMutBufSize -> FOut HeapNode GCMutBufSize -> GC ()
mark mutUpdateIn comUpdateIn = do
  handlePop
  noDescend <- not <$> use descend
  write <- commitPrevToWork
  wl    <- use worklist
  prevAddr .= Nothing
  pickOp >>= \case

    MarkFetchBuffer -> if size mutUpdateIn > 0
      then do
        let as = nodeChildren . fromJust $ read mutUpdateIn
        gcMemOut  .:= write :> repeat RamNoOp
        heapLatch .= Right (appLen as, as)
        mutUpdatePop .:= True
      else do
        let as = nodeChildren . fromJust $ read comUpdateIn
        gcMemOut  .:= write :> repeat RamNoOp
        heapLatch .= Right (appLen as, as)
        comUpdatePop .:= True

    MarkFetchQueue -> do -- Fetch contents of worklist head from shared heap
        gcMemOut   .:= RamWrite (_llHead wl) (Marked True) :> repeat RamNoOp
        -- ^ Looks like we should also include `write` but if that is a write,
        -- it is guaranteed to conflict with our current marking.
        worklist .= if isNoOp write || noDescend
          then wl & llLen %~ (\x -> x-1)
                  & llUpdate .~ True
          else wl & llLen %~ (\x -> x-1)
                  & llUpdate .~ False
                  & llHead .~ getPtr write
        heapLatch .= Left (_llHead wl)
        workLatch .= Nothing

    MarkFinish -> do
      next <- use postMark
      gcMemOut  .:= write :> repeat RamNoOp
      phase .= fromMaybe SweepSparks next
      prevAddr .= Nothing
      postMark .= Nothing
      -- updatePop .:= True
      -- ^ Clears mut buffer if the mutator does an update at the
      -- last possible second. If we leave it there, it'll be marked
      -- on the next pass...

    MarkChild -> nextChild >>= \case
      Nothing -> gcMemOut .:= RamNoOp :> write :> repeat RamNoOp
      Just (work,c)  ->
        if collision c write
          then do
            gcMemOut .:= RamNoOp :> write :> repeat RamNoOp
            prevAddr .= Nothing
          else do
            gcMemOut .:= RamRead c :> write :> repeat RamNoOp
            workLatch .= Nothing
            prevAddr .= Just c
            descend .= work
            -- TODO Make a choice based on work/mark destination

  where
    collision x (RamWrite y _) = x==y
    collision _ _              = False

    isNoOp RamNoOp = True
    isNoOp _       = False

    getPtr (RamWrite _ n) = tailPtr n
    getPtr _              = undefined

    -- Should a given post-marking phase fully mark, or just put mutation buffer elements on the work queue?
    fullMark Nothing          = True
    fullMark (Just SweepRefs) = True
    fullMark _                = False

    pickOp = do
      wl    <- use worklist
      mutSz <- use mutBufSz
      post  <- use postMark
      child <- either (const 0) fst <$> use heapLatch
      if child > minBound
        then pure MarkChild
        else if mutSz > 0
          then pure MarkFetchBuffer
          else if _llLen wl > 0 && fullMark post
            then pure MarkFetchQueue
            else pure MarkFinish

    handlePop = do
      wlPrev <- use worklist
      when (_llUpdate wlPrev) $ do
           prev <- fromJust <$> use workLatch
           worklist .= (wlPrev & llUpdate .~ False
                               & llHead   .~ tailPtr prev)

    markable x = (isRight x,) <$> either heapAddr heapAddr x
    nextChild = do
      (i,as) <- fromRight (0, repeat Nothing) <$> use heapLatch
      let rs = imap (\n a -> if resize n < i
                        then fmap (n,) (markable =<< a)
                        else Nothing) as
          r = fold (<|>) $ reverse rs
      heapLatch .= Right (maybe 0 (resize . fst) r, as)
      pure $ snd <$> r

data RootsOp
  = RootsConc -- ^ Search from the bottom of the stack up, concurrent to the
              -- mutator. We need to stop once we get dangerously close to the
              -- stack pointer.
  | RootsSync -- ^ Wait for the mutator to pause in a stable state so we can
              -- look at the topmost stack elements
  | RootsSeq     -- ^ Search remaining snoopable stack elements while mutator is paused.
  | RootsSeqTops -- ^ Search topmost, cached stack elements while mutator is paused.
  | RootsFinish
  deriving (Generic, NFDataX, Show, ShowX, Eq)

-- | Find mutator's graph roots from its stack before marking the heap
gatherRoots :: Maybe (Vec CMaxPush Atom) -> StackState -> HeapAddr -> GC ()
gatherRoots mtops (n, a) tsoAddr = do
  write <- commitPrevToWork
  rsp <- use rootSp
  case pickOp rsp of
    RootsConc -> findRoot rsp write a
    RootsSync -> do
      gcMemOut .:= RamNoOp :> write :> repeat RamNoOp
      prevAddr .= Nothing
      worklist %= (llUpdate .~ True)
    RootsSeq  -> findRoot rsp write a
    RootsSeqTops ->
      let i = n - rsp - 1
      in findRoot rsp write (fromJust mtops !! i)
    RootsFinish -> do
      worklist %= (llUpdate .~ False)
      -- On the last cycle, we also mark the current TSO address as live
      gcMemOut .:= RamRead tsoAddr :> write :> repeat RamNoOp
      rootSp .= 0
      prevAddr .= Just tsoAddr
      workLatch .= Nothing
      phase    .= RootsUStk 1
      heapLatch .= Right (0, repeat Nothing)
      -- TODO Try to add current tso here too.
  where

    -- Derive current operation
    pickOp rsp
      | not $ nearStackPtr n rsp
      = RootsConc
      | rsp >= n
      = RootsFinish
      | otherwise
      = case mtops of
          Nothing -> RootsSync
          Just _  -> if rsp + snatToNum (SNat @CMaxPush) < n
           then RootsSeq
           else RootsSeqTops

    -- Check given stack element for a root address, and dispatch booking memory ops.
    findRoot rsp write b = do
      let rsp' = 1 + rsp
      rootSp        .= rsp'
      case heapAddr b of
        Nothing -> do
          prevAddr .= Nothing
          gcMemOut .:= RamNoOp :> write :> repeat RamNoOp
        Just r -> do
          pa <- use prevAddr
          if Just r == pa
            then do
              prevAddr .= Nothing
              gcMemOut .:= RamNoOp :> write :> repeat RamNoOp
            else do
              let fetch = RamRead r
              prevAddr .= Just r
              workLatch .= Nothing
              gcMemOut .:= fetch :> write :> repeat RamNoOp

jumpToMarking :: Maybe Phase -> GC ()
jumpToMarking p = do
  heapLatch .= Right (0, repeat Nothing)
  workLatch .= Nothing
  phase .= Mark
  postMark .= p

gatherRootsBlocked :: Bool -> Maybe ThreadResult -> GC ()
gatherRootsBlocked ready ret = do
  write <- commitPrevToWork
  gcMemOut .:= RamNoOp :> write :> repeat RamNoOp -- Default is to update previous entry
  prevAddr .= Nothing
  waiting >>= \case
    -- Waiting for response
    True -> case ret of
      -- Nothing to do yet, make sure we update any previous entries
      Nothing -> pure ()
      Just (SparkElem _ _) -> failure ErrGcSparkDuringTsoRoots
      -- Got a response
      Just (TsoElem final' mtso) -> do
        let done  = final' || isNothing mtso
        let fetch = maybe RamNoOp RamRead mtso
        when ready $ do
          prevAddr .= mtso
          gcMemOut .:= fetch :> write :> repeat RamNoOp
          when (isJust mtso) $
            workLatch .= Nothing
          threadRetPop .:= True
          if done
            then resetWait >> phase .= RootsReady
            else resetWait
    False -> do
      sz <- use mutBufSz
      if sz > 0
        then jumpToMarking (Just RootsBlocked) -- Switch to marking to clear mutation buffer (avoids blocking)
        else when ready askNext -- Not waiting, get next
  where
    waiting = (/=0) <$> use rootSp
    resetWait = rootSp .= 0
    setWait = rootSp .= 1
    askNext = do
      setWait
      threadCmd .:= Just PeekBlocked

gatherRootsReady :: Bool -> Maybe ThreadResult -> GC ()
gatherRootsReady ready ret = do
  write <- commitPrevToWork
  gcMemOut .:= RamNoOp :> write :> repeat RamNoOp -- Default is to update previous entry
  prevAddr .= Nothing
  waiting >>= \case
    -- Waiting for response
    True -> case ret of
      -- Nothing to do yet, make sure we update any previous entries
      Nothing -> pure ()
      Just (SparkElem _ _) -> failure ErrGcSparkDuringTsoRoots
      -- Got a response
      Just (TsoElem final' mtso) -> do
        let done  = final' || isNothing mtso
        let fetch = maybe RamNoOp RamRead mtso
        when ready $ do
          prevAddr .= mtso
          gcMemOut .:= fetch :> write :> repeat RamNoOp
          when (isJust mtso) $
            workLatch .= Nothing
          threadRetPop .:= True
          if done
            then resetWait >> phase .= RootsRef
            else resetWait
    False -> do
      sz <- use mutBufSz
      if sz > 0
        then jumpToMarking (Just RootsReady) -- Switch to marking to clear mutation buffer (avoids blocking)
        else when ready askNext -- Not waiting, get next
  where
    waiting = (/=0) <$> use rootSp
    resetWait = rootSp .= 0
    setWait = rootSp .= 1
    askNext = do
      setWait
      threadCmd .:= Just PeekReady
      -- TODO Huge amount of overlap with gatherRootsBlocked. Abstract it.

gatherRootsRef :: Bool -> Maybe (Maybe (HeapAddr, RefCountNode)) -> GC ()
gatherRootsRef ready ret = do
  write <- commitPrevToWork
  gcMemOut .:= RamNoOp :> write :> repeat RamNoOp -- Default is to update previous entry
  prevAddr .= Nothing
  waiting >>= \case
    -- Waiting for response
    True -> case ret of
      -- Nothing to do yet, make sure we update any previous entries
      Nothing -> pure ()
      -- End of list
      Just Nothing  -> finish
      -- Got a response
      Just (Just (addr, RefCountNode ga count _)) -> do
        let fetch = if count > 0 then RamRead addr else RamNoOp
        when ready $ do
          if isNothing ga && count == 0
            then do
              rcCmd .:= Just RCFree -- Remove elements that no longer have external references
              rcRetPop .:= True
              resetWait
            else do
              gcMemOut .:= fetch :> write :> repeat RamNoOp
              if count > 0
                then do
                  prevAddr .= Just addr
                  workLatch .= Nothing
                else
                  prevAddr .= Nothing
              rcRetPop .:= True
              resetWait
    False -> do
      sz <- use mutBufSz
      if sz > 0
        then jumpToMarking (Just RootsRef) -- Switch to marking to clear mutation buffer (avoids blocking)
        else when ready askNext -- Not waiting, get next
  where
    waiting = (/=0) <$> use rootSp
    resetWait = rootSp .= 0
    setWait = rootSp .= 1
    askNext = do
      setWait
      rcCmd .:= Just RCTraverse
    finish = do
      rcRetPop .:= True
      resetWait
      jumpToMarking Nothing
      -- TODO Some amount of overlap with gatherRootsBlocked. Abstract it? Maybe?

-- TODO We shouldn't take an extra cycle to wait for askNext calls. Instead, make a new state where we transition to Mark while handling any commitToPrev calls.

sweepSparks :: Bool -> Maybe ThreadResult -> GC ()
sweepSparks ready ret = waiting >>= \case
  -- Waiting for response
  True -> case ret of
    Nothing -> pure ()
    Just (TsoElem _ _) -> failure ErrGcTsoDuringSparkSweep
    -- Got a response
    Just (SparkElem final' msp) -> case msp of
      -- Finished traversal, move onto next phase
      Nothing -> done
      -- Need to collect unconditionally
      Just (_addr, Fizzled) -> when ready $ do
        threadRetPop .:= True
        threadCmd .:= Just CollectSpark
        resetWait
        when final' done
      -- Conditionally collect
      Just (addr, Sparked) -> use prevAddr >>= \case
        -- Need to chech GC memory for status
        Nothing -> do
          gcMemOut .:= RamRead addr :> repeat RamNoOp
          workLatch .= Nothing
          prevAddr .= Just addr
        -- Decide based on status
        Just _ -> do
          gcnode <- fromJust <$> use workLatch
          unless (isMarked gcnode) $
            threadCmd .:= Just CollectSpark
          threadRetPop .:= True
          prevAddr  .= Nothing
          resetWait
          when final' done
      Just (_, Ready)   -> failure ErrGcTsoDuringSparkSweep
      Just (_, Blocked) -> failure ErrGcTsoDuringSparkSweep
  False -> do
    sz <- use mutBufSz
    if sz > 0
      then jumpToMarking Nothing -- Switch to marking to clear mutation buffer (avoids blocking)
                                 -- `Nothing` will ensure we do a complete mark, not just add things to the worklist.
      else when ready askNext -- Not waiting, get next
  where
    waiting = (/=0) <$> use rootSp
    resetWait = rootSp .= 0
    setWait = rootSp .= 1
    askNext = do
      setWait
      threadCmd .:= Just PeekSpark
    done = do
      sz <- use mutBufSz
      resetWait
      threadRetPop .:= True
      if sz > 0
        then jumpToMarking (Just SweepRefs) -- Switch to marking to clear mutation buffer (avoids blocking)
        else do phase .= SweepRefs

sweepRefs :: Bool -> Maybe (Maybe (HeapAddr, RefCountNode)) -> GC ()
sweepRefs ready ret = waiting >>= \case
  -- Waiting for response
  True -> case ret of
    Nothing -> pure ()
    -- Finished traversal, move onto next phase
    Just Nothing -> done
    -- Got a response
    Just (Just (addr,rc)) -> if _rcCount rc == 0
      -- Conditionally collect
      then if isNothing (_rcSrc rc)
        then when ready $ do
          rcCmd .:= Just RCFree -- Remove elements that no longer have external references
          rcRetPop .:= True
          resetWait
        else use prevAddr >>= \case
          -- Need to chech GC memory for status
          Nothing -> do
            gcMemOut .:= RamRead addr :> repeat RamNoOp
            workLatch .= Nothing
            prevAddr .= Just addr
          -- Decide based on status
          Just _ -> when ready $ do
            gcnode <- fromJust <$> use workLatch
            unless (isMarked gcnode) $
              rcCmd .:= Just RCFree
            rcRetPop .:= True
            prevAddr  .= Nothing
            resetWait
      -- Continue
      else do
        rcRetPop .:= True
        prevAddr  .= Nothing
        resetWait
  False -> when ready askNext -- Not waiting, get next
  where
    waiting = (/=0) <$> use rootSp
    resetWait = rootSp .= 0
    setWait = rootSp .= 1
    askNext = do
      setWait
      rcCmd .:= Just RCTraverse
    done = do
      phase .= SweepHeap (Just 0)
      rcRetPop .:= True
      resetWait

-- TODO Rework the logic that sets GA in RC memory. We only want to save it when
-- we want to decrement owner RC on free (i.e. when that holds an indirection?)

gatherRootsUStk :: Index UStkSize -> GC ()
gatherRootsUStk x = do
  write <- commitPrevToWork
  gcMemOut .:= RamNoOp :> write :> repeat RamNoOp -- Default is to update previous entry
  prevAddr .= Nothing
  use ustkEnd >>= \case
    True -> finish -- Finished through early exit
    False -> do -- Keep going
      l <- use ustkLatch
      go x write l
  where
    finish = do
      phase .= RootsBlocked
      ustkLatch .= Nothing
      ustkEnd .= False

    -- Assert ustk read  (l = Nothing                       )
    go addr _write Nothing = do
      prevAddr  .= Nothing
      ustkOpLatch .= RamRead addr
    -- Dispatch gc read  (l = Just Left ; prevAddr = Nothing)
    go addr write (Just u) = case uAddr u of
      Nothing -> errorX "There shouldn't be any Nothing updates now?!" -- Already nullified, skip to next
      Just a  -> do
        prevAddr .= Just a
        gcMemOut .:= RamRead a :> write :> repeat RamNoOp
        workLatch .= Nothing
        ustkLatch .= Nothing
        if addr == maxBound
          then finish
          else phase .= RootsUStk (succ addr)

-- | Push an unmarked `_prevAddr` onto the workqueue. This must first be read
-- and latched in `_workLatch` so we can ensure that the address does not
-- already appear in the workqueue. Cycles would damage our linked-list
-- implementation.
commitPrevToWork :: GC (RamOp HeapSize GCNode)
commitPrevToWork = use prevAddr >>= \case
  Nothing -> return RamNoOp
  Just addr -> do
    wl <- use workLatch
    d  <- use descend
    commit addr wl d
  where
    commit a node deeply
      -- When not descending into a new addr
      | not deeply && node == Just Unmarked = writeMarked a
      -- When descending into a new or shallowly marked addr
      | deeply && (node == Just Unmarked || node == Just (Marked False)) = pushwl a
      -- Nothing useful
      | otherwise = pure RamNoOp
    writeMarked a = do
      descend .= True
      pure $ RamWrite a (Marked False)
    pushwl a = do
      wl   <- use worklist
      let op = RamWrite a (WorkQueue $ _llHead wl)
      worklist .= (wl & llLen  %~ (+1)
                      & llHead .~ a
                  )
      return op

-- | Whenever we get a response from a heap operation or a workqueue read, latch
-- it.
latchReads :: Maybe HeapNode -> HeapOut GCNode MaxAps HeapSize -> GC ()
latchReads heapMemIn gcMemIn = do
  when (isJust heapMemIn) $
       let as = nodeChildren $ fromJust heapMemIn in
       heapLatch .= Right (appLen as, as)
  workLatch %= Just . fromMaybe (head $ read gcMemIn)

-- | Whenever a freelist has been used in an allocation, update its head pointer
-- to the next free address.
updateNexts :: HeapOut GCNode MaxAps HeapSize -> GC ()
updateNexts gcMemIn = do
  fl <- use freelist
  when (or $ zipWith badFree (_reads gcMemIn) fl) $
    failure ErrGcBadFree
  freelist .= zipWith upd (_reads gcMemIn) fl
  where
    isFree (FreeList _) = True
    isFree _            = False
    badFree a f = _llUpdate (snd f) && not (isFree a)
    upd a f
      | _llUpdate (snd f)
      = second (\l ->l & llUpdate .~ False
                       & llHead   .~ tailPtr a)
               f
      | otherwise = f

-- | Whenever we get a ustk snoop respose, latch the answer and deassert any operations
updateUStk :: Update -> Bool -> Bool -> GC ()
updateUStk uin uret uwarn
  -- Got to end
  | uwarn = do
      ustkEnd .= True
      ustkLatch .= Nothing
      ustkOpLatch .= RamNoOp
  | uret = do
      ustkEnd .= False
      ustkLatch .= Just uin
      ustkOpLatch .= RamNoOp
  | otherwise = pure ()

-- | Is there an immediate deallocation request that should be handled on this cycle?
deallocSafe :: Phase -> HeapAddr -> Bool
deallocSafe Mark        _  = False
deallocSafe RootsBlocked _ = False
deallocSafe RootsReady   _ = False
deallocSafe RootsRef     _ = False
deallocSafe _ _            = True

-- | Update bookkeeping to reflect new allocations
alloc :: Vec MaxAps Bool -> Bool -> GC ()
alloc as bubble = do
  as' <- if bubble
           then balanceFreelists <&> rotateRight as
           else pure as
  fl <- use freelist
  p  <- use phase
  let addrs = zipWith (\b x -> if b then Just (_llHead $ fst x) else Nothing) as' fl
      fl' = zipWith upd addrs fl
      ops = map (memOp p) addrs

  pa <- use prevAddr
  when (isJust pa && elem pa addrs)
       (workLatch .= Just (freshNode p (fromJust pa)))

  gcMemOut .:= ops
  freelist  .= fl'
  where
    memOp p = maybe RamNoOp (\y -> RamWrite y $ freshNode p y)
    upd (Just _) f
      = swp $ first (\l -> l & llLen %~ (\z->z-1)  & llUpdate .~ True) f
    upd Nothing f = f
    freshNode Idle _ = Unmarked
    freshNode RootsStack _ = Unmarked
    freshNode (SweepHeap Nothing) _ = Unmarked
    freshNode (SweepHeap (Just saddr)) addr =
      if addr < satPred SatZero saddr then Unmarked else Marked False
    freshNode _  _ = Marked False

-- | Update bookkeeping to reflect immediate deallocations
free :: HeapAddr -> Bool -> GC ()
free a bubble = do
  unproductivePasses .= 0
  memOp <- onSmallestList (push a) bubble
  gcMemOut .:= memOp :> repeat RamNoOp
  pa <- use prevAddr
  when (Just a == pa)
       (workLatch .= Just (wrData memOp))
  where
    wrData (RamWrite _ x) = x
    wrData _              = undefined

-- | Initialise bookkeeping memory by putting all addresses into balanced freelists
initFreelists :: HeapAddr -> GC ()
initFreelists n
  | n >= halfLength
  = phase .= Idle
  | otherwise
  = gcMemOut .:= RamWrite n              (FreeList (succ' n)             ) :>
                 RamWrite (n+halfLength) (FreeList (succ' $ n+halfLength)) :>
                 Nil >>
    phase .= Init (n+1)
  where
    halfLength = snatToNum (SNat @(HeapSize `Div` 2))
    succ' = unpack . (+1) . pack

redistribute :: GC ()
{-| Tries to actively redistribute the freelist elements.

Since we are sometimes forced to push onto the seconds (when there is no bubble
signal), this redistribute function acts as a counterbalance. We'll move
-}
redistribute = do
  fl <- use freelist
  let (fl', memops) = unzip $ map redist fl
  freelist .= fl'
  gcMemOut .:= memops
  where
    redist (a,b)
      | _llLen a < _llLen b
      = ( (a & llLen    %~ succ
             & llHead   .~ _llHead b
          ,b & llLen    %~ pred
             & llUpdate .~ True
          )
        , RamWrite (_llHead b) (FreeList $ _llHead a)
        )
      | otherwise
      = ((a,b), RamNoOp)

-- | Try to balance the lengths of our multiple freelists when there is an
-- allocation bubble detected. This works be reordering freelists only -- the
-- first pair is more likely to be used. See `redistribute` to actually move
-- elements between freelists within each pair for active balancing.
balanceFreelists :: GC (Index MaxAps)
balanceFreelists = do
  (r,fl) <- sortLists <$> use freelist
  freelist .= fl
  pure r
  where
    sortLists :: FreeLists -> (Index MaxAps, FreeLists)
    sortLists (a:>b:>Nil)
      | pairLen b > pairLen a = (1, b:>a:>Nil)
      | otherwise             = (0, a:>b:>Nil)
    sortLists x = errorX $ unwords
      [ "Heron.Core.Collector.balanceFreelists: Freelist has unexpected shape: "
      , show x
      ]

swp :: (b, a) -> (a, b)
swp (a,b) = (b,a)

both :: Bifunctor p => (c -> d) -> p c c -> p d d
both f = bimap f f

pairLen :: (GCList, GCList) -> HeapAddr
pairLen = uncurry (+) . both _llLen

{-
Note [Free list balancing]
~~~~~~~~~~~~~~~~~~~~~~~~~~

Since we keep four independent free lists, we need to consider how to keep them
reasonably well balanced. The lists are grouped by channels (a tuple of two
lists). The idea is that, since popping from a free list takes one cycle, the
mutator can choose to look at the other element in the tuple when the first is
invalid. We then maintain multiple channels, one for each heap application the
mutator can allocate simultaneously.

As I currently understand it, the main sources of imbalance are:

  1. The mutator is more likely to allocate from the first channel than the
  second.

  2. Unless we have detected a "bubble" cycle, we can only push new elements
  onto the second element in each channel (although we _do_ have choice of
  _which_ channel).

Essentially, the both elements of the first channel are more likely to be
depleted (1), and the second elements of every channel are more likely to grow
fat (2).

We try to tackle (1) by, during a bubble cycle, we can choose to swap the
channels depending on their relative lengths. This is quite cheap and does not
require any access to the bookkeeping memory.

Issue (2) is tackled in two ways. Whenever we perform a push during a bubble
cycle, the smallest single list is targeted. Also, while idle, we can actively
redistribute the elements of a free list during bubbles. This is applied to each
channel in isolation. If the second list is longer than the first, we pop its
head and push it onto the first list.

This seems to work quite well but we currently have quite poor visibility of
list balance. For our large examples, it would be worthwhile to monitor free
list lengths in a verilator simulation.
-}
