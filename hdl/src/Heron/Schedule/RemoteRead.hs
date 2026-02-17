{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-} -- For unused generated lenses

{-
-}
module Heron.Schedule.RemoteRead where

import           Barbies.TH
import           Clash.Prelude          hiding (read)
import           Control.Lens           hiding (Index, assign, at, both, imap,
                                         op, (:>))
import           Control.Monad
import           Control.Monad.Extra    (whenM)
import           Data.Maybe
import           Heron.Core.Heap        ()
import           Heron.Schedule.Context (ContextCmd (..))
import           Heron.Schedule.Fetch   (FetchCmd (..))
import           Heron.Schedule.Fish    (FishCmd)
import           Heron.Schedule.Put     (PutCmd (..))
import           Heron.Schedule.Types
import           Heron.Template
import           RetroClash.Barbies
import           RetroClash.CPU         hiding (update)

declareBareB [d|
  data RemoteReadIn = RemoteReadIn
    { readPorts  :: Vec 4 ReadPort
    , remoteWriteReady :: Bool
    , putReady         :: Bool
    , localWriteReady  :: Bool
    , fetchReady       :: Bool
    , fishReady        :: Bool
    , contextReady     :: Bool
    , refCountReady    :: Bool
    , coreId           :: CoreId
    , priorityReset    :: Bool
    } |]
deriving instance Generic (Pure RemoteReadIn)
deriving instance NFDataX (Pure RemoteReadIn)
deriving instance BitPack (Pure RemoteReadIn)
deriving instance Show    (Pure RemoteReadIn)
deriving instance ShowX   (Pure RemoteReadIn)

data RemoteReadState = RemoteReadState
  { _priority :: PortId
  , _op       :: Maybe ComMsg
  }
  deriving (Generic, NFDataX, Show, ShowX, BitPack)
makeLenses ''RemoteReadState

declareBareB [d|
  data RemoteReadOut = RemoteReadOut
    { _readPortsPop :: Vec 4 Bool
    , _remoteWritePush :: Maybe ComMsg
    , _putPush         :: Maybe PutCmd
    , _localWritePush  :: Maybe ComMutRequest
    , _fetchPush       :: Maybe FetchCmd
    , _fishPush        :: Maybe FishCmd
    , _contextPush     :: Maybe ContextCmd
    , _refCountPush    :: Maybe RefCountCmd
    } |]
makeLenses ''RemoteReadOut
deriving instance Generic (Pure RemoteReadOut)
deriving instance NFDataX (Pure RemoteReadOut)
deriving instance BitPack (Pure RemoteReadOut)
deriving instance Show    (Pure RemoteReadOut)
deriving instance ShowX   (Pure RemoteReadOut)

initState :: RemoteReadState
initState = RemoteReadState
  { _priority = PUp
  , _op = Nothing
  }

defaultOutput :: RemoteReadState -> Pure RemoteReadOut
defaultOutput _ = RemoteReadOut
  { _readPortsPop = repeat False
  , _remoteWritePush = Nothing
  , _putPush         = Nothing
  , _localWritePush  = Nothing
  , _fetchPush       = Nothing
  , _fishPush        = Nothing
  , _contextPush     = Nothing
  , _refCountPush    = Nothing
  }

type ComRemoteRead = CPUM RemoteReadState RemoteReadOut

-- | Synchronous control logic, packaged as a Mealy machine
comRemoteRead :: (HiddenClockResetEnable dom)
    => Signals dom RemoteReadIn -> Signals dom RemoteReadOut
comRemoteRead = mealyCPU initState defaultOutput step

step :: Pure RemoteReadIn -> ComRemoteRead ()
step RemoteReadIn{..} = do
  -- Find next
  whenM (isNothing <$> use op) $ do
    offset <- use priority
    let (offset', mmsg) = portsRead (if priorityReset then PUp else offset) readPorts
    priority .= offset'
    case mmsg of
      Nothing      -> pure ()
      Just (p,msg) -> op .= Just msg >> (readPortsPop .:= popRead p)

  -- Dispatch
  use op >>= \case
    Nothing -> pure ()
    Just c -> do
      let cid' = coreId
      if forwardable cid' c
        then when remoteWriteReady $ do
          finish
          remoteWritePush .:= Just c
        else case c of
          MFish src fuel -> when allReady $ do
            finish
            fishPush .:= Just (fuel,src)
          MFizzled _ -> when allReady $ do
            finish
            localWritePush .:= Just CMFizzled
          MFetch dst tso src -> when allReady $ do
            finish
            fetchPush .:= Just (FetchCmd tso dst src)
          MPut indr dst src n -> when allReady $ do
            finish
            putPush  .:= Just (PutCmd indr src (snd dst) n)
          MSuspend dst rest -> when allReady $ do
            finish
            contextPush .:= Just (Suspend (snd dst) rest)
          MUnblock dst -> when allReady $ do
            finish
            contextPush .:= Just (Resume $ snd dst)
          MCollected dst -> when allReady $ do
            finish
            refCountPush .:= Just (RCModify Nothing (-1) $ snd dst)
          MFail src e -> when allReady $ do
            finish
            localWritePush .:= Just (CMFail src e)
  where
    finish = op .= Nothing
    allReady = fishReady && localWriteReady && fetchReady && putReady && contextReady && refCountReady

forwardable :: CoreId -> ComMsg -> Bool
forwardable cid = maybe False ((/=cid) . fst) . routeAddr

routeAddr :: ComMsg -> Maybe GlobalAddr
routeAddr (MFish          _ _) = Nothing
routeAddr (MFizzled   dst    ) = Just dst
routeAddr (MFetch     dst _ _) = Just dst
routeAddr (MPut    _  dst _ _) = Just dst
routeAddr (MSuspend   dst _  ) = Just dst
routeAddr (MUnblock   dst    ) = Just dst
routeAddr (MCollected dst    ) = Just dst
routeAddr (MFail       _  _  ) = Just ((0,0),0)
