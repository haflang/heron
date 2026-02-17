{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-} -- For unused generated lenses

{-
-}
module Heron.Schedule.Fetch where

import           Barbies.TH
import           Clash.Prelude        hiding (read)
import           Control.Lens         hiding (Index, assign, at, both, children,
                                       imap, op, (:>))
import           Control.Monad
import           Data.Maybe
import           Heron.Core.Heap      ()
import           Heron.Parameters
import           Heron.Schedule.Types
import           Heron.Template
import           Heron.TraceFSM
import           RetroClash.Barbies
import           RetroClash.CPU       hiding (update)


data Phase
  = Init
  | Reading
  | Redirect HeapNode -- TODO Should these heap nodes be moved into FetchState?
  | Replying ComMsg
  | AccountRefs ComMsg
  | Block (Maybe GlobalAddr)
  | Suspend (Maybe GlobalAddr)
  | SparkAgain GlobalAddr
  deriving (Generic, NFDataX, Show, ShowX, Eq, BitPack)

type Fuel = Unsigned (CLog 2 MaxCores)
type FishCmd = (Fuel, SrcAddr)

data FetchCmd = FetchCmd TSOAddr DstAddr SrcAddr
  deriving (Generic, NFDataX, Show, ShowX, Eq, BitPack)

declareBareB [d|
  data FetchIn = FetchIn
    { cmd    :: Maybe FetchCmd
    , sparkCmd :: Maybe FetchCmd
    , remoteWriteReady :: Bool
    , fishReady :: Bool
    , threadsReady :: Bool
    , rcReady :: Bool
    , heapIn :: Maybe HeapNode
    , mutUpdate :: Maybe HeapAddr
    , updateReady :: Bool
    , coreId :: CoreId
    } |]
deriving instance Generic (Pure FetchIn)
deriving instance NFDataX (Pure FetchIn)
deriving instance BitPack (Pure FetchIn)
deriving instance Show    (Pure FetchIn)
deriving instance ShowX   (Pure FetchIn)

data FetchState = FetchState
  { _phase    :: Phase
  , _sparking :: Bool
  , _children :: Vec NodeLen (Maybe HeapAddr)
  }
  deriving (Generic, NFDataX, Show, ShowX, BitPack, AutoReg)
makeLenses ''FetchState

declareBareB [d|
  data FetchOut = FetchOut
    { _remoteWritePush :: Maybe ComMsg
    , _fishPush :: Maybe FishCmd
    , _cmdPop :: Bool
    , _sparkCmdPop :: Bool
    , _heapOut :: Maybe (RamOp HeapSize HeapNode)
    , _clearHeap :: Bool
    , _fizzle :: Maybe HeapAddr
    , _rcCmd :: Maybe RefCountCmd
    , _updatePush :: Maybe HeapNode
    } |]
makeLenses ''FetchOut
deriving instance Generic (Pure FetchOut)
deriving instance NFDataX (Pure FetchOut)
deriving instance BitPack (Pure FetchOut)
deriving instance Show    (Pure FetchOut)
deriving instance ShowX   (Pure FetchOut)

initState :: FetchState
initState = FetchState
  { _phase = Init
  , _sparking = False
  , _children = repeat Nothing
  }

defaultOutput :: FetchState -> Pure FetchOut
defaultOutput _ = FetchOut
  { _remoteWritePush = Nothing
  , _fishPush = Nothing
  , _cmdPop = False
  , _sparkCmdPop = False
  , _heapOut = Nothing
  , _clearHeap = False
  , _fizzle = Nothing
  , _rcCmd = Nothing
  , _updatePush = Nothing
  }

type ComFetch = CPUM FetchState FetchOut

-- | Synchronous control logic, packaged as a Mealy machine
comFetch :: (HiddenClockResetEnable dom)
    => Signal dom CoreId -> Signals dom FetchIn -> Signals dom FetchOut
comFetch = traceFSM "_com_fetch" initState defaultOutput step

step :: Pure FetchIn -> ComFetch ()
step FetchIn{..} =  do
  FetchCmd tso (_,addr) src <- decode <$> use sparking
  p <- use phase
  -- TODO Be really careful about what happens when we reset Fetch due to mutUpdate... I haven't given this enough thought.
  if volatile addr && dangerousPhase p
    then do phase .= Reading
            let Just (FetchCmd _ (_,a) _) = sparkCmd <|> cmd
            heapOut .:= Just (RamRead a)
            clearHeap .:= True
    else go tso addr src
  where
    cid = coreId
    dangerousPhase Init            = False
    dangerousPhase Reading         = False
    dangerousPhase (Redirect _)    = True
    dangerousPhase (Block _)       = True
    dangerousPhase (AccountRefs _) = False
    dangerousPhase (Replying _)    = False
    dangerousPhase (Suspend _)     = False
    dangerousPhase (SparkAgain _)  = False
    -- TODO Is the issue where dispatch before transitioning to the new state?
    decode True  = fromMaybe (FetchCmd 0 ((0,0),maxBound) ((0,0),0)) sparkCmd
    decode False = fromMaybe (FetchCmd 0 ((0,0),maxBound) ((0,0),0)) cmd
    volatile a = Just a == mutUpdate
    go tso addr src = do
        use phase >>= \case
          -- TODO Is it always OK to prioritise sparking requests? Round robin might be better?
          Init -> case sparkCmd of
            Just (FetchCmd _ (_,a) _) -> do
              phase .= Reading
              sparking .= True
              heapOut .:= Just (RamRead a)
            Nothing -> case cmd of
              Just (FetchCmd _ (_,a) _) -> do
                phase .= Reading
                sparking .= False
                heapOut .:= Just (RamRead a)
              Nothing -> pure ()
          Reading -> case heapIn of
            Nothing -> when (volatile addr) retryRead -- If we latch a partial result while this address is mutated, restart
            Just n -> if volatile addr
              then retryRead
              else clear >> case n of -- TODO Do we need the volatile check here too?
                Locked mb -> do
                  sp <- use sparking
                  if sp then fishAgain src
                        else block (fst src, tso) mb
                  -- TODO Do we also  want to fail sparks on fetchme? Probably...
                FetchMe ga -> if fst ga == fst src
                  then do
                    startAccountRefs (MPut False src (cid,addr) n)
                  else redirect (FetchMe ga)
                    -- Switched this branch to redirection instead of replying.
                    -- Fetching this implies an intent to evaluate, and we'll be better off looking in the correct place on the first try.
                    -- Also prevents duplication of references.
                _ -> tryThis src n
          Redirect node -> when (isJust heapIn && threadsReady && updateReady) $ do
            fizzle .:= Just addr
            clearHeap .:= True
            updatePush .:= Just node -- Need to make GC aware of the update if it's marking
                                     -- BUG we only signal the change sometime _after_ writing the indirection to the heap... needs to be atomic
            startAccountRefs (MPut True src (cid,addr) node)
          AccountRefs m -> accountRefs m
          Replying m -> tryReply m
          Block tailb -> when (isJust heapIn) $ do
            clearHeap .:= True
            suspend tailb
          Suspend t -> suspend t
          SparkAgain ga -> fishAgain ga
      where

      retryRead = do
        when (isJust heapIn) $ do
          clearHeap .:= True
          heapOut   .:= Just (RamRead addr)

      clear = do
        clearHeap .:= True

      fishAgain ga = do
        phase .= SparkAgain ga
        when fishReady $ do
          fishPush .:= Just (0, ga) -- TODO We should maintain the fuel properly
          phase .= Init
          sparkCmdPop .:= True

      tryThis ga n = do
        isSparking <- use sparking
        if isWHNF n
          then if isSparking
                 then fishAgain ga -- An untimely update fizzled this spark
                 else startAccountRefs (MPut False ga (cid,addr) n)
          else redirect n

      block newb tailb
        | Just newb == tailb = suspend Nothing
        -- ^ If we retry this because of an update, _we_ maybe wrote tailb ourselves. Check if it matches the newb, and skip as needed to avoid cycles.
        | otherwise = do
            phase .= Block tailb
            heapOut .:= Just (RamWrite addr (Locked $ Just newb))

      tryReply m = do
        when (remoteWriteReady && updateReady) $ do
          phase .= Init
          remoteWritePush .:= Just m
          use sparking >>= \case
            True  -> sparkCmdPop .:= True >>
                     updatePush .:= sparkedNode m
                     -- Signal to GC that this address is now live
                     -- This might be new GC information if the program has speculative paralleism
            False -> cmdPop .:= True
      sparkedNode (MPut {}) = Just $ App True 1 (Just (Ptr PShared addr) :> repeat Nothing)
      sparkedNode _ = Nothing

      startAccountRefs m@(MPut _ _ _ n) = do
        children .= case n of
          App  _ _ cs -> map (maybe Nothing heapAddr) cs
          Case _ _ cs -> map (maybe Nothing heapAddr) cs ++ repeat Nothing
          _           -> repeat Nothing
        phase .= AccountRefs m
      startAccountRefs _ = errorX "Tried to do fetch's reference accounting on a message that wasn't MPut"

      accountRefs m@(MPut indr dst _ _) = do
        phase .= AccountRefs m
        child <- fold (<|>) <$> use children
        case child of
          Nothing -> when rcReady $ do -- No more children left to incr, decrement this!
            s <- use sparking
            let offset = if s then 0 else (-1)
            let newGA = if indr then Just (Just dst) else Nothing
            rcCmd .:= Just (RCModify newGA offset addr)
            phase .= Replying m
          Just c  -> when (rcReady && updateReady) $ do -- Increment next child addr
            rcCmd .:= Just (RCModify Nothing 1 c)
            updatePush .:= Just (App False 1 $ Just (Ptr PShared c) :> repeat Nothing)
            -- ^ This might make a node a new root. Let the GC know
            children %= map (maybe Nothing (\a -> if c == a then Nothing else Just a))
      accountRefs _ = errorX "Unexpected message in accountRefs --- should be MPut"

      redirect n = do
        heapOut .:= Just (RamWrite addr (FetchMe src))
        phase .= Redirect n

      suspend tailb = do
        phase .= Suspend tailb
        when remoteWriteReady $ do
          phase .= Init
          remoteWritePush .:= Just (MSuspend (fst src, tso) tailb)
          sp <- use sparking
          if sp then sparkCmdPop .:= True else cmdPop .:= True
