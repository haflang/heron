{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-} -- For unused generated lenses

module Heron.Schedule.Threads where

import           Barbies.TH
import           Clash.Prelude        hiding (read)
import           Control.Lens         hiding (Index, assign, at, both, imap, op,
                                       (:>))
import           Control.Monad
import           Control.Monad.Extra
import           Data.Maybe
import           Heron.Core.Heap      ()
import           Heron.Parameters
import           Heron.Schedule.Types
import           Heron.Template
import           Heron.TraceFSM
import           RetroClash.Barbies
import           RetroClash.CPU       hiding (update)

data ComPhase
  = Init
  | Simple
  | Unblocking
  | Readying
  deriving (Generic, NFDataX, Show, ShowX, Eq, BitPack)

data ThreadLL = ThreadLL
  { _hd     :: Maybe HeapAddr
  , _tl     :: Maybe HeapAddr
  , _sz     :: HeapAddr
  , _popped :: Bool -- Have we popped from list during this cycle?
                    -- Used to update tail pointer at start of next cycle
  , _pushed :: Bool -- Have we pushed to list during this cycle?
                    -- Used to correct behaviour when push-before-pop
                    -- TODO Do we really need this?
  }
  deriving (Generic, NFDataX, Show, ShowX, BitPack)
makeLenses ''ThreadLL

data ThreadTraversal = ThreadTraversal
  { _travNext     :: Maybe HeapAddr
  , _travPrev     :: Maybe HeapAddr
  , _travProgress :: Bool
  , _travLatch    :: Bool
  }
  deriving (Generic, NFDataX, Show, ShowX, BitPack)
makeLenses ''ThreadTraversal

type ThreadNode = (ThreadTag, Maybe HeapAddr)
type ThreadRamOp = (RamOp HeapSize ThreadTag, RamOp HeapSize (Maybe HeapAddr))

declareBareB [d|
  data ThreadIn = ThreadIn
    { comFetchCmd :: Maybe ThreadCmd
    , comFishCmd :: Maybe ThreadCmd
    , comContextCmd :: Maybe ThreadCmd
    , mutCmd :: Maybe ThreadCmd
    , gcCmd  :: Maybe ThreadCmd
    , memComIn :: ThreadNode
    , memAuxIn :: ThreadNode
    } |]
deriving instance Generic (Pure ThreadIn)
deriving instance NFDataX (Pure ThreadIn)
deriving instance BitPack (Pure ThreadIn)
deriving instance Show    (Pure ThreadIn)
deriving instance ShowX   (Pure ThreadIn)

data ThreadState = ThreadState
  { _phase         :: ComPhase
  , _llSpark       :: ThreadLL
  , _llReady       :: ThreadLL
  , _llBlock       :: ThreadLL
  , _sparking      :: Maybe HeapAddr
  , _sparksCleanup :: Bool
  , _unblockAddr   :: HeapAddr
  , _finger        :: HeapAddr
  , _knuckle       :: Maybe HeapAddr -- It's slightly below the finger...
  , _travSpark     :: ThreadTraversal
  , _travReady     :: ThreadTraversal
  , _travBlock     :: ThreadTraversal
  , _sparkRelink   :: Maybe HeapAddr
  , _auxOut        :: ThreadRamOp
  , _comLatch      :: Maybe ThreadNode
  }
  deriving (Generic, NFDataX, Show, ShowX, BitPack, AutoReg)
makeLenses ''ThreadState

initState :: ThreadState
initState = ThreadState
  { _phase = Init
  , _llSpark = ThreadLL Nothing Nothing 0 False False
  , _llReady = ThreadLL Nothing Nothing 0 False False
  , _llBlock = ThreadLL Nothing Nothing 0 False False
  , _sparking = Nothing
  , _sparksCleanup = False
  , _unblockAddr = 0
  , _finger = 0
  , _knuckle = Nothing
  , _travSpark = ThreadTraversal Nothing Nothing False False
  , _travReady = ThreadTraversal Nothing Nothing False False
  , _travBlock = ThreadTraversal Nothing Nothing False False
  , _sparkRelink = Nothing
  , _auxOut = (RamNoOp, RamNoOp)
  , _comLatch = Nothing
  }

declareBareB [d|
  data ThreadOut = ThreadOut
    { _doneResume :: Bool
    , _outGc  :: Maybe ThreadResult
    , _nextReady :: Maybe TSOAddr
    , _nextSpark :: Maybe (ThreadTag, HeapAddr)
    , _memCom :: ThreadRamOp
    , _memAux :: ThreadRamOp
    , _numSparks :: HeapAddr
    -- Keeping the two ports seperate lets us rely on `defaultOutput` more heavily
    , _fetchPop :: Bool
    , _fishPop :: Bool
    , _contextPop :: Bool
    , _gcPop :: Bool
    } |]
makeLenses ''ThreadOut
deriving instance Generic (Pure ThreadOut)
deriving instance NFDataX (Pure ThreadOut)
deriving instance BitPack (Pure ThreadOut)
deriving instance Show    (Pure ThreadOut)
deriving instance ShowX   (Pure ThreadOut)

defaultOutput :: ThreadState -> Pure ThreadOut
defaultOutput s = ThreadOut
  { _doneResume = False
  , _outGc  = Nothing
  , _nextReady = _tl (_llReady s)
  , _nextSpark = Nothing
  , _memCom = (RamNoOp, RamNoOp)
  , _memAux = _auxOut s
  , _numSparks = _sz (_llSpark s)
  , _fetchPop = False
  , _fishPop = False
  , _contextPop = False
  , _gcPop = False
  }

type ComThread = CPUM ThreadState ThreadOut

-- | Synchronous control logic, packaged as a Mealy machine
comThread :: (HiddenClockResetEnable dom)
    => Signal dom CoreId -> Signals dom ThreadIn -> Signals dom ThreadOut
comThread = traceFSM "_com_thread" initState defaultOutput step

updateLL :: Maybe HeapAddr -> ThreadLL -> ThreadLL
updateLL newTl ThreadLL{..}
  | _sz == 0  = ThreadLL{_tl = Nothing, _popped = False, _pushed = False, ..} -- sz is already decremented, so don't worry
  | _popped   = ThreadLL{_tl = newTl  , _popped = False, _pushed = False, ..}
  | otherwise = ThreadLL{_pushed = False, ..}

latchTrav :: Maybe HeapAddr -> ThreadLL -> Maybe HeapAddr -> Maybe HeapAddr
latchTrav newTl ThreadLL{..} travPtr
  | _sz == 0  = Nothing
  | _popped  && travPtr == _tl = newTl
  | otherwise = travPtr
  -- TODO The travPtr is really a tag for the thing that we're currently
  -- fetching. After output, it's then used as the "next" pointer. We should
  -- really be updating the "next pointer" usage.

popLL :: ThreadLL -> (ThreadLL, ThreadRamOp)
popLL ThreadLL{..}
  | _sz == 0 = errorX "Threads tried to pop from empty LL"
  | _sz == 1 =  ( ThreadLL{_sz = 0, _popped = False, _hd = Nothing, _tl = Nothing, ..}
                , (RamRead (fromJust _tl), RamNoOp)
                )
  | otherwise = ( ThreadLL{_sz = pred _sz, _popped = True, ..}
                , (RamRead (fromJust _tl), RamRead (fromJust _tl))
                )
                -- TODO Is it OK that we leave the old tail dangling until the
                -- start of the next cycle? Probably not.
-- TODO What about push before pop on one cycle?

pushLL :: ThreadTag -> HeapAddr -> ThreadLL -> (ThreadLL, ThreadRamOp)
pushLL tag a ThreadLL{..}
  | _sz == 0 =
    ( ThreadLL{_sz = 1, _hd = Just a, _tl = Just a, _pushed = True, ..}
    , (RamWrite a tag, RamNoOp)
    )
  | otherwise =
    ( ThreadLL{_sz = succ _sz, _hd = Just a, _pushed = True, ..}
    , (RamWrite a tag, RamWrite (fromJust _hd) $ Just a)
    )

relinkLL :: Maybe HeapAddr -> HeapAddr -> Maybe HeapAddr -> ThreadLL -> (ThreadLL, ThreadRamOp)
relinkLL prev here next ThreadLL{..}
  -- We're removing the head
  | _hd == Just here =
      ( ThreadLL{_sz=sz', _hd = prev, ..}
      , (RamNoOp, RamNoOp)
      )
  -- We're removing the tail
  | isNothing prev =
      ( ThreadLL{_sz=sz', _tl = next, ..}
      , (RamNoOp, RamNoOp)
      )
  -- We're removing an intermediate element
  | otherwise =
      ( ThreadLL{_sz=sz', ..}
      , (RamNoOp, RamWrite (fromJust prev) next)
      )
  where sz' = pred _sz
  -- TODO Without GC this only happens with block list. With GC we need to worry
  -- about conflicts with com's push/pops too.

relinkTrav :: Maybe HeapAddr -> HeapAddr -> Maybe HeapAddr -> ThreadLL -> ThreadTraversal -> (ThreadTraversal, Maybe ThreadResult)
relinkTrav prev here next ThreadLL{..} ThreadTraversal{..}
  -- We're removing the head
  | _hd == Just here && _travNext == _hd = if _travLatch
    -- If we're currently peeking, stop it and reply with end message
    then ( ThreadTraversal{_travNext = Nothing, _travPrev = Nothing, _travLatch = False, _travProgress = False}
         , Just $ TsoElem True Nothing
         )
    -- If we're waiting to read it, set ptr to Nothing
    else ( ThreadTraversal{_travNext = Nothing, ..}, Nothing )
  -- We're removing the tail
  | isNothing prev && _travNext == Just here && not _travLatch =
      ( ThreadTraversal{_travNext = next, ..}, Nothing )
  | isNothing prev && _travPrev == Just here =
      ( ThreadTraversal{_travPrev = Nothing, ..}, Nothing )
  -- We're removing an intermediate element
  | _travNext == Just here && not _travLatch =
      ( ThreadTraversal{_travNext = next, ..}, Nothing )
  | _travPrev == Just here =
      ( ThreadTraversal{_travPrev = prev, ..}, Nothing )
  | otherwise =
      ( ThreadTraversal{..}, Nothing )

-- TODO This feels like it should be split into versions specialised for sparks vs ready/blocked
outputTrav :: Bool -> ThreadNode -> Maybe HeapAddr -> ThreadLL -> ThreadTraversal -> ComThread ThreadTraversal
outputTrav sparks nexts newTail ThreadLL{..} ThreadTraversal{..}
  -- Need to output a result (don't worry about pop conflicts here, we're essentially popping anyway --- words that will come back to bite me, probably)
  | _travLatch = do
      if sparks
        then outGc .:= Just (SparkElem finished $ Just (addr, fst nexts))
        else outGc .:= Just (TsoElem   finished $ Just addr)
      -- Track sparks relink ptr
      when sparks $
        if _travPrev == _tl && _popped
          then sparkRelink .= Nothing -- If the tail has just been popped and we're looking at it, drop the relink addr
          else sparkRelink .= _travPrev
      pure $ if finished
        then ThreadTraversal{_travNext = snd nexts, _travPrev = _travNext, _travProgress = False, _travLatch = False}
        else ThreadTraversal{_travNext = snd nexts, _travPrev = _travNext, _travProgress = True , _travLatch = False}
  -- Need to make sure that traversal pointers are bumped forward with other pops
  | _popped = do
      let llRestart = ThreadTraversal {_travNext = newTail, _travPrev = Nothing, ..}
      let llCont    = ThreadTraversal {..}
      link <- use sparkRelink
      let recover
            | _tl == _travNext = do
                sparkRelink .= Nothing
                pure llRestart
            | _tl == _travPrev = do
                sparkRelink .= Nothing
                pure llRestart
            | _tl == link = do
                sparkRelink .= Nothing
                pure llCont
            | otherwise = pure llCont
            -- Maybe we need to handle newTail == Nothing or size == 0?
      recover

  -- If we thought we were nearly finished, be careful of new pushes
  | _pushed && isNothing _travNext =
    pure $ ThreadTraversal {_travNext = _hd, ..}
  | isNothing _tl =
    pure $ ThreadTraversal {_travNext = Nothing, _travPrev = Nothing, ..}
    -- TODO What about consistency with travPrev being popped?
  | otherwise = pure $ ThreadTraversal{..}
  where
    addr = fromJust _travNext
    finished = _travNext == _hd

updateTrav
  :: (ThreadLL -> ThreadTraversal -> ComThread ThreadTraversal)
  -> Getter ThreadState ThreadLL
  -> Lens' ThreadState ThreadTraversal
  -> ComThread ()
updateTrav f ll tr = do
  l <- use ll
  t <- use tr
  t' <- f l t
  tr .= t'

step :: Pure ThreadIn -> ComThread ()
step ThreadIn{..} = do
  auxOut .= (RamNoOp, RamNoOp)
  comLatch %= (<|> Just memComIn)

  -- Output latest sparks
  sp <- use sparking
  nextSpark .:= fmap (fst memComIn,) sp
  sparking .= Nothing

  -- Update traversal states
  updateTrav (outputTrav True  memAuxIn (snd memComIn)) llSpark travSpark
  updateTrav (outputTrav False memAuxIn (snd memAuxIn)) llReady travReady
  updateTrav (outputTrav False memAuxIn (snd memComIn)) llBlock travBlock

  -- Update LLs after pops
  llSpark %= updateLL (snd memComIn) -- Com is the spark popper
  llReady %= updateLL (snd memAuxIn) -- Mut is the ready popper
  llBlock %= updateLL (snd memComIn) -- Com is the block popper

  -- Arbitrate between mut and gc commands, giving priority to mut.
  --
  -- This turns out to be surprisingly easy. The game here is to always service
  -- mut (all operations take one cycle anyway.) When we get a moment, we
  -- accept GC work. This means we need to latch GC outputs and keep them
  -- consistent with the state held in the memory. Luckily the set of commands
  -- for mut and gc are disjoint, so we don't need to distinguish between them
  -- upfront. Popping from the right input is done based on context.

  doAux (mutCmd <|> gcCmd)
  auxes <- use auxOut
  comRead <- fromJust <$> use comLatch

  use phase >>= \case
    Init       -> doInit
    Simple     -> selectComOp comFetchCmd comFishCmd comContextCmd >>= \case
      Just cmd -> unlessM (conflict auxes <$> comAddrs cmd) $ do
        sparksCleanup .= False
        popComOp comFetchCmd comFishCmd comContextCmd
        doCom cmd
      Nothing  -> if isNothing (mutCmd <|> gcCmd)
        then pure () -- TODO removed for debug doCleanup comRead
        else sparksCleanup .= False
    Unblocking -> unlessM (conflict auxes <$> comAddrsUnblock comRead) $
      doUnblock comRead
    Readying   -> unlessM (conflict auxes <$> comAddrsReadying) doReadying

selectComOp :: Maybe ThreadCmd -> Maybe ThreadCmd -> Maybe ThreadCmd -> ComThread (Maybe ThreadCmd)
selectComOp comFetchCmd comFishCmd comContextCmd
  | isJust comFetchCmd = pure comFetchCmd
  | isJust comFishCmd = pure comFishCmd
  | isJust comContextCmd = pure comContextCmd
  | otherwise = pure Nothing

popComOp :: Maybe ThreadCmd -> Maybe ThreadCmd -> Maybe ThreadCmd -> ComThread ()
popComOp comFetchCmd comFishCmd comContextCmd
  | isJust comFetchCmd   = fetchPop .:= True
  | isJust comFishCmd    = fishPop .:= True
  | isJust comContextCmd = contextPop .:= True
  | otherwise = pure ()

-- Check for address conflicts between aux port (priority) and com port. If
-- there are conflicts, we'll stall the com port operation.
conflict :: ThreadRamOp -> (Maybe HeapAddr, Maybe HeapAddr) -> Bool
conflict auxs coms = check (f (fst auxs)) (fst coms) ||
                     check (f (snd auxs)) (snd coms)
  where
    check (Just x) (Just y) = x==y
    check _ _               = False
    f RamNoOp        = Nothing
    f (RamRead a)    = Just a
    f (RamWrite a _) = Just a

-- We need to predict which addresses we'll access in order to check for
-- conflicts with the aux port
comAddrs :: ThreadCmd -> ComThread (Maybe HeapAddr, Maybe HeapAddr)
comAddrs PopSpark = use llSpark >>= \l ->
  pure (_tl l, _tl l)
comAddrs (PushBlock a) = use llBlock >>= \l ->
  pure (Just a, _hd l)
comAddrs (FizzleSpark a) =
  pure (Just a, Nothing)
comAddrs (Unblock _) = use llBlock >>= \l ->
  pure (Nothing, _tl l)
comAddrs x = errorX $ unwords ["Unexpected ThreadCmd in comAddrs ", show x]

comAddrsReadying :: ComThread (Maybe HeapAddr, Maybe HeapAddr)
comAddrsReadying = do
  h <- _hd <$> use llReady
  x <- use unblockAddr
  pure (Just x, h)

comAddrsUnblock :: ThreadNode -> ComThread (Maybe HeapAddr, Maybe HeapAddr)
comAddrsUnblock (_,next) = do
  here <- use finger
  target <- use unblockAddr
  x <- use knuckle
  if here==target
    then pure (Nothing, x)
    else pure (Nothing, next)

doInit :: ComThread ()
doInit = do
  a <- use finger
  let a' = succ a
  memCom .:= (RamWrite a  Fizzled, RamWrite a  Nothing)
  memAux .:= (RamWrite a' Fizzled, RamWrite a' Nothing)
  if a' == maxBound
    then phase .= Simple
    else finger %= (2+)

doPeek
  :: ThreadResult
  -> Getter ThreadState ThreadLL
  -> Lens' ThreadState ThreadTraversal
  -> ComThread ()
doPeek endRet ll t = do
  ThreadTraversal{..} <- use t
  if _travProgress
    then maybe doEnd doElem _travNext
    else use ll >>= (\case
      -- The list was empty to start with
      -- TODO Since we start from the tail, we might need to handle elements that are _pushed_ onto the head while we work?
      Nothing -> doEnd
      -- Start from the tail
      Just a  -> do
        sparkRelink .= Nothing
        t %= set travPrev Nothing . set travNext (Just a)
        doElem a) . _tl
  where
    doElem a = do
      auxOut .= (RamRead a, RamRead a)
      t %= set travLatch True
      gcPop .:= True
    doEnd = do
      t %= (set travProgress False   .
            set travNext     Nothing .
            set travPrev     Nothing .
            set travLatch    False   )
      outGc .:= Just endRet
      gcPop .:= True
    -- TODO Elements can be removed from the middle of block list... be careful
    -- about consistency. Repeated reads is OK but missing one is bad

doAux :: Maybe ThreadCmd -> ComThread ()
doAux Nothing = pure ()
-- Mutator ops
doAux (Just PopReady) = do
  (ll', op) <- popLL <$> use llReady
  llReady .= ll'
  auxOut .= op
doAux (Just (PushSpark a)) = do
  oldHd <- _hd <$> use llSpark
  ts <- use travSpark
  updTravHead oldHd a ts
  (ll', op) <- pushLL Sparked a <$> use llSpark
  llSpark .= ll'
  auxOut .= op
  -- BUG Can the same thing be sparked more than once? Probably... but it usually shouldn't?
  where
    -- If we're pushing onto the head when traversal is also at the head, note it down
    -- TODO Does this fix also apply to other linked lists? I guess that the GC only collect from ref list too...
    updTravHead oldhd newhd t
      | _travPrev t == oldhd = travSpark .= t {_travNext = Just newhd}
      | otherwise = pure ()
doAux (Just (FizzleSpark a)) =
  auxOut .= (RamWrite a Fizzled, RamNoOp)
-- GC ops
doAux (Just PeekBlocked) = doPeek (TsoElem   True Nothing) llBlock travBlock
doAux (Just PeekReady)   = doPeek (TsoElem   True Nothing) llReady travReady
doAux (Just PeekSpark)   = doPeek (SparkElem True Nothing) llSpark travSpark
doAux (Just CollectSpark) = do
  prev <- use sparkRelink
  ThreadTraversal next here _ _ <- use travSpark
  gcPop .:= True
  case here of
    -- Already been popped from list, continue
    Nothing -> pure ()
    Just a  -> do
      (ll', op) <- relinkLL prev a next <$> use llSpark
      travSpark %= set travPrev prev
      -- TODO Do we need to tidy up trav pointers more than this? We'll do one read before the next collect spark...
      llSpark .= ll'
      auxOut  .= op
      -- TODO Do we need to worry about conflicts with doCom access to spark pool? Probably
doAux (Just _) = errorX "Unexpected mut op in threads"

-- If we have two consecutive idle cycles, we can clean up fizzled sparks on the spark pool
doCleanup :: ThreadNode -> ComThread ()
doCleanup n = use sparksCleanup >>= \case
  True -> do
    sparksCleanup .= False
    when (fst n == Fizzled) $ do
      ll <- use llSpark
      whenM ((_tl ll ==) <$> use sparkRelink) $
        sparkRelink .= Nothing
      let (ll', op) = popLL ll
      llSpark .= ll'
      memCom .:= op
  False -> do
    ll <- use llSpark
    if _sz ll > 0
      then do
        sparksCleanup .= True
        memCom .:= (RamRead (fromJust $ _tl ll), RamNoOp)
        comLatch .= Nothing
      else sparksCleanup .= False


doCom :: ThreadCmd -> ComThread ()
doCom PopSpark = do
  ll <- use llSpark
  if _sz ll == 0
    -- Something went wrong due to latencies, and now there are no sparks left
    -- TODO This is pretty worrying, what other bugs can creep in because of this?
    then
      nextSpark .:= Just (Fizzled, 0)
    else do
      sparking .= _tl ll
      whenM ((_tl ll ==) <$> use sparkRelink) $
        sparkRelink .= Nothing
      let (ll', op) = popLL ll
      llSpark .= ll'
      memCom .:= op
doCom (PushBlock a) = do
  (ll', op) <- pushLL Blocked a <$> use llBlock
  llBlock .= ll'
  memCom .:= op
doCom (FizzleSpark a) =
  memCom .:= (RamWrite a Fizzled, RamNoOp)
doCom (Unblock a) = do
  phase .= Unblocking
  unblockAddr .= a
  start <- fromJust . _tl <$> use llBlock
  finger .= start
  knuckle .= Nothing
  memCom .:= (RamNoOp, RamRead start)
  comLatch .= Nothing
doCom _ = errorX "Unexpected mut op in threads"

doReadying :: ComThread ()
doReadying = do
  a <- use unblockAddr
  (ll', op) <- pushLL Ready a <$> use llReady
  llReady .= ll'
  memCom .:= op
  phase .= Simple
  doneResume .:= True -- We're done, let COM know it can continue

doUnblock :: ThreadNode -> ComThread ()
doUnblock (_,next) = do
  here <- use finger
  prev <- use knuckle
  target <- use unblockAddr
  if here == target
    then do -- We found it, tie up the LL and continue with pushing onto ready list
      lb <- use llBlock
      -- Accounting for the block list traversal state
      (t, tOp)  <- relinkTrav prev here next lb <$> use travBlock
      travBlock .= t
      -- BUG Not sure about the below...
      when (isJust tOp) $ do
        gcPop .:= True -- TODO Can here conflix with doAux?
        outGc .:= tOp
      -- Relink the block list
      let (ll', op) = relinkLL prev here next lb
      llBlock .= ll'
      memCom .:= op
      phase .= Readying
    else do -- Nope, traverse further
      memCom .:= (RamNoOp, RamRead $ fromJust next)
      comLatch .= Nothing
      knuckle .= Just here
      finger  .= fromJust next


-- TODO Could make idlining fish clearing better by traversing the whole list,
-- not just the
