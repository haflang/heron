{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-} -- For unused generated lenses

module Heron.Schedule.RefCounts where

import           Barbies.TH
import           Clash.Prelude        hiding (read)
import           Control.Lens         hiding (Index, assign, at, both, imap,
                                       modifying, op, (:>))
import           Control.Monad
import           Data.Maybe
import           Heron.Core.Heap      ()
import           Heron.Parameters
import           Heron.Schedule.Types
import           Heron.Template
import           Heron.TraceFSM
import           RetroClash.Barbies
import           RetroClash.CPU       hiding (update)

data ComPhase
  = ComIdle
  | ComWModify
  deriving (Generic, NFDataX, Show, ShowX, Eq, BitPack)

data GCPhase
  = GCIdle
  | GCRead
  | GCWClear
  | GCWModify
  deriving (Generic, NFDataX, Show, ShowX, Eq, BitPack)

data ComSrc
  = ComFetch
  | ComPut
  | ComRemote
  deriving (Generic, NFDataX, Show, ShowX, Eq, BitPack)

type RefCountRamOp = RamOp HeapSize RefCountNode

declareBareB [d|
  data RefCountIn = RefCountIn
    { comCmd   :: Maybe RefCountCmd
    , putCmd   :: Maybe RefCountCmd
    , remoteCmd :: Maybe RefCountCmd
    ,  gcCmd   :: Maybe RefCountCmd
    , comMemIn :: RefCountNode
    ,  gcMemIn :: RefCountNode
    , remoteWriteReady :: Bool
    } |]
deriving instance Generic (Pure RefCountIn)
deriving instance NFDataX (Pure RefCountIn)
deriving instance BitPack (Pure RefCountIn)
deriving instance Show    (Pure RefCountIn)
deriving instance ShowX   (Pure RefCountIn)

data RefCountState = RefCountState
  { _comPhase    :: ComPhase
  ,  _gcPhase    ::  GCPhase
  , _initialise  :: Bool
  , _comOp       :: Maybe RefCountCmd
  , _gcOp        :: Maybe RefCountCmd
  , _comSrc      :: ComSrc -- Are we processing a Put request, Fetch request, or remote free on the COM port?
  , _finger      :: HeapAddr
  , _knuckle     :: Maybe HeapAddr -- It's slightly below the finger...
  , _fingerNode  :: Maybe RefCountNode
  , _knuckleNode :: Maybe RefCountNode
  , _traversing  :: Bool
  , _clearAddr   :: HeapAddr -- The address that needs to be nuked by a Free
  , _llTail      :: Maybe HeapAddr -- We track tail so we know when a node with a "Nothing" tail is actually a member of the list or not.
  , _llHead      :: Maybe HeapAddr -- Only pushing to the head means that we can cons
                              -- using one mem op only. How does this affect GC
                              -- traversal though, are all elements added after
                              -- root ID starts still marked?
  }
  deriving (Generic, NFDataX, Show, ShowX, BitPack, AutoReg)
makeLenses ''RefCountState

initState :: RefCountState
initState = RefCountState
  { _comPhase = ComIdle
  , _gcPhase  = GCIdle
  , _initialise = True
  , _comOp    = Nothing
  , _gcOp     = Nothing
  , _comSrc  = ComFetch
  , _finger   = 0
  , _knuckle  = Nothing
  , _fingerNode = Nothing
  , _knuckleNode = Nothing
  , _clearAddr = 0
  , _traversing = False
  , _llHead   = Nothing
  , _llTail   = Nothing
  }

declareBareB [d|
  data RefCountOut = RefCountOut
    { _nextRef  :: Maybe (Maybe (HeapAddr, RefCountNode))
    , _gcMem  :: RefCountRamOp
    , _comMem :: RefCountRamOp
    -- Keeping the two ports seperate lets us rely on `defaultOutput` more heavily
    , _comPop :: Bool
    , _putPop :: Bool
    , _remotePop :: Bool
    , _gcPop :: Bool
    , _remoteWritePush :: Maybe ComMsg
    } |]
makeLenses ''RefCountOut
deriving instance Generic (Pure RefCountOut)
deriving instance NFDataX (Pure RefCountOut)
deriving instance BitPack (Pure RefCountOut)
deriving instance Show    (Pure RefCountOut)
deriving instance ShowX   (Pure RefCountOut)

defaultOutput :: RefCountState -> Pure RefCountOut
defaultOutput _ = RefCountOut
  { _nextRef = Nothing
  , _gcMem = RamNoOp
  , _comMem = RamNoOp
  , _comPop = False
  , _putPop = False
  , _remotePop = False
  , _gcPop  = False
  , _remoteWritePush = Nothing
  }

type ComRefCount = CPUM RefCountState RefCountOut

-- | Synchronous control logic, packaged as a Mealy machine
comRefCount :: (HiddenClockResetEnable dom)
    => Signal dom CoreId -> Signals dom RefCountIn -> Signals dom RefCountOut
comRefCount = traceFSM "_com_rc" initState defaultOutput step

type NewGlobalAddr = Maybe (Maybe GlobalAddr)

modifying :: RefCountCmd -> Maybe (NewGlobalAddr, Signed 2, HeapAddr)
modifying (RCModify ga n a) = Just (ga, n, a)
modifying _                 = Nothing

popCom :: ComRefCount ()
popCom = do
  comOp .= Nothing
  use comSrc >>= \case
    ComPut -> putPop .:= True
    ComFetch -> comPop .:= True
    ComRemote -> remotePop .:= True

popGc :: ComRefCount ()
popGc = do
  gcOp .= Nothing
  gcPop .:= True

adjust :: RefCount -> Signed 2 -> RefCount
adjust x y = x + bitCoerce (resize y)

-- Update ref count
-- COM port only ever does Modify commands.
-- We either push onto head (also setting tail when empty), or RMW in place.
-- Returns any node written to finger
doCom :: RefCountNode -> RefCountCmd -> ComRefCount ()
doCom comMemIn (RCModify ga' n a) = use comPhase >>= \case
  ComIdle -> do
    comMem .:= RamRead a
    comPhase .= ComWModify
  ComWModify -> do
    hd <- use llHead
    tl <- use llTail
    let (RefCountNode ga rc rest) = comMemIn
    let ga'' = fromJust (ga' <|> Just ga)
    popCom
    comPhase .= ComIdle

    if isNothing rest && Just a /= tl
      -- Not a member of the LL already, push onto head
      then do
        let newNode = RefCountNode ga'' (adjust rc n) hd
        llHead .= Just a
        llTail %= (<|> Just a) -- TODO Conflicts with GC clear op, maybe?
        commit a newNode
        -- If the GC traversal is looking at the head, we need to commit this new node as the knuckle
        fingeringHead <- (==hd) . Just <$> use finger
        when fingeringHead $ do
          knuckle .= Just a
          knuckleNode .= Just newNode
      -- Already in list, update in place
      else do
        commit a (RefCountNode ga'' (adjust rc n) rest)
  where
    commit addr node = do
      f <- use finger
      k <- use knuckle
      when (addr      == f) $ fingerNode .= Just node
      when (Just addr == k) $ knuckleNode .= Just node
      comMem .:= RamWrite addr node
doCom _ _ = errorX "Unexpected ref count opertaion on COM port"

{-

1) Com may have read and is expecting the latest value to appear on comMemIn next cycle:
2) Com may have updated head (and possibly tail) & written a new node that wasn't already on the list. Some clobbering of f/k nodes too.
3) Com may have written an existing node in-place. Some clobbering of f/k.

We use a URAM with GC on port B. Can swap if we want though.

| R | W | old | -   |
| W | R | -   | new |
| W | W | explode.. |

If com reads, we need to tell it that we've written
If com writes the same as us, boom!

-}

-- GC Port can Traverse or Free, both are two-cycle operations
-- TODO doGC needs to
-- be in charge of restarting/pausing when com is doing something dangerous.
-- Consider write addresses and tail address too.
doGc :: Bool -> Maybe HeapAddr -> RefCountNode -> RefCountCmd -> ComRefCount ()
-- Readout possibly referenced addrs
-- Traverses from head through to tail pointers.
doGc _ _ gcMemIn RCTraverse = use gcPhase >>= \case
  GCIdle -> do
    nextAddr >>= \case
      Just addr -> do
        finger .= addr
        fingerNode .= Nothing
        gcMem .:= RamRead addr
        gcPhase .= GCRead
      Nothing -> do
        traversing .= False
        nextRef .:= Just Nothing
        popGc
  GCRead -> do
    addr <- use finger
    nextRef .:= Just (Just (addr, gcMemIn))
    fingerNode %= (<|> Just gcMemIn) -- Prioritises writes done in COM
    gcPhase .= GCIdle
    popGc
  _ -> errorX "Unexpected GC phase in RefCounts while traversing"
  where
    nextAddr = use traversing >>= \case
      -- Start from the head
      False -> do
        traversing .= True
        knuckle .= Nothing
        knuckleNode .= Nothing
        use llHead
      -- Or follow tail of current node
      True -> use fingerNode >>= \case
        Just n@(RefCountNode _ _ rest) -> do
          addr <- use finger
          knuckle .= Just addr
          knuckleNode .= Just n
          pure rest
        Nothing -> errorX "RC didn't find a finger node when traversing"

-- Free an address that is no longer referenced
-- BUG What if COM has just added a new node, or overwritten one whose tail points here?
doGc remoteWriteReady mutModification _ RCFree = use gcPhase >>= \case
  -- Tie up tail pointers
  GCIdle -> do
    (RefCountNode parent count rest) <- fromJust <$> use fingerNode
    -- If the RC was incremented between traversal request and free request, don't free it!
    -- BUG Maybe we need this check in GCWClear too?
    if count > 0
      then popGc
      else do
        when (isNothing parent || remoteWriteReady) $ do
          -- Freeing the head
          use knuckle >>= \case
            Nothing -> do -- We're removing the head
              when (isJust parent) $
                remoteWritePush .:= Just (MCollected $ fromJust parent)
              llHead .= rest -- Can we ever be wrong about this? Will knuckle be wrong?
              addr <- use finger
              llTail %= \t -> if t == Just addr then rest else t
              gcPhase .= GCWClear
              traversing .= False -- Popped head, so we need to restart traversal
              clearAddr .= addr
            -- Freeing something in tail
            Just prevAddr -> unless (mutModification == Just prevAddr) $ do
              -- BUG what com modification are we sensitive to?
              when (isJust parent) $
                remoteWritePush .:= Just (MCollected $ fromJust parent)
              (RefCountNode ga n _) <- fromJust <$> use knuckleNode
              let wr = RefCountNode ga n rest
              gcMem .:= RamWrite prevAddr wr
              when (isNothing rest) $ -- If removing the last element, fix up the ll tail pointer
                llTail .= Just prevAddr
              gcPhase .= GCWClear
              -- Need to revert traversal pointers.
              use finger >>= (clearAddr .=)
              finger .= prevAddr
              prevNode <- fromJust <$> use knuckleNode
              knuckle .= _rcTail prevNode
              fingerNode .= Just wr
  -- Clear node
  GCWClear -> do
    popGc
    gcPhase .= GCIdle
    addr <- use clearAddr
    gcMem .:= RamWrite addr (RefCountNode Nothing 0 Nothing)
  _ -> errorX "Unexpected GC phase in RefCounts while freeing"

doInit :: ComRefCount ()
doInit = do
  a <- use finger
  let a' = succ a
  comMem .:= RamWrite a  (RefCountNode Nothing 0 Nothing)
  gcMem  .:= RamWrite a' (RefCountNode Nothing 0 Nothing)
  if a' == maxBound
    then initialise .= False
    else finger %= (2+)

step :: Pure RefCountIn -> ComRefCount ()
step RefCountIn{..} = do

  -- Setup com operation
  prevComOp <- use comOp
  comCmd' <- case prevComOp of
    Nothing -> if isJust putCmd
                 then comSrc .= ComPut >> pure putCmd
                 else if isJust comCmd
                   then comSrc .= ComFetch >> pure comCmd
                   else comSrc .= ComRemote >> pure remoteCmd
    Just x -> pure $ Just x
  comOp .= comCmd'

  -- Setup gc operation
  gcOp %= (<|> gcCmd)

  use initialise >>= \case
    True -> doInit
    False -> do
      -- Handle com ops
      use comOp >>= \case
        Nothing -> pure ()
        Just x  -> doCom comMemIn x

      -- Handle gc ops
      let conflicts = (fmap (\(_,_,a)->a) . modifying) =<< comCmd'
      use gcOp >>= \case
        Nothing -> pure ()
        Just x  -> doGc remoteWriteReady conflicts gcMemIn x
