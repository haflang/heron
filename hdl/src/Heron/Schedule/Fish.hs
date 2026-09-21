{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-} -- For unused generated lenses

{-
-}
module Heron.Schedule.Fish where

import           Barbies.TH
import           Clash.Prelude        hiding (read)
import           Control.Lens         hiding (Index, assign, at, both, imap, op,
                                       (:>))
import           Control.Monad
import           Control.Monad.Extra  (whenM)
import           Data.Maybe
import           Heron.Core.Heap      ()
import           Heron.Parameters
import           Heron.Schedule.Fetch (FetchCmd (..))
import           Heron.Schedule.Types
import           Heron.Template
import           Heron.TraceFSM
import           RetroClash.Barbies
import           RetroClash.CPU       hiding (update)

data Source
  = Local
  | Remote
  | Retry
  deriving (Generic, NFDataX, Show, ShowX, Eq, BitPack)

data Phase
  = Init
  | Read
  | Dispatch
  | Fail
  deriving (Generic, NFDataX, Show, ShowX, Eq, BitPack)

type Fuel = Unsigned (CLog 2 MaxCores)
type FishCmd = (Fuel, SrcAddr)
type LocalCmd = HeapAddr

declareBareB [d|
  data FishIn = FishIn
    { cmd      :: Maybe FishCmd
    , localCmd :: Maybe LocalCmd
    , fetchCmd :: Maybe FishCmd
    , remoteWriteReady :: Bool
    , localWriteReady :: Bool
    , fetchReady :: Bool
    , threadsReady :: Bool
    , numSparks :: HeapAddr
    , sparks :: Maybe (ThreadTag, HeapAddr)
    , mutUpdate :: Maybe HeapAddr
    , coreId :: CoreId
    } |]
deriving instance Generic (Pure FishIn)
deriving instance NFDataX (Pure FishIn)
deriving instance BitPack (Pure FishIn)
deriving instance Show    (Pure FishIn)
deriving instance ShowX   (Pure FishIn)

data FishState = FishState
  { _phase :: Phase
  , _op    :: Maybe (Either LocalCmd FishCmd)
  , _opSrc :: Source
  , _spark :: HeapAddr
  }
  deriving (Generic, NFDataX, Show, ShowX, BitPack, AutoReg)
makeLenses ''FishState

declareBareB [d|
  data FishOut = FishOut
    { _remoteWritePush :: Maybe ComMsg
    , _localWritePush :: Maybe ComMutRequest
    , _fetchCmdPush   :: Maybe FetchCmd
    , _cmdPop :: Bool
    , _localCmdPop :: Bool
    , _fetchCmdPop :: Bool
    , _sparksPop :: Bool
    } |]
makeLenses ''FishOut
deriving instance Generic (Pure FishOut)
deriving instance NFDataX (Pure FishOut)
deriving instance BitPack (Pure FishOut)
deriving instance Show    (Pure FishOut)
deriving instance ShowX   (Pure FishOut)

initState :: FishState
initState = FishState
  { _phase = Init
  , _op = Nothing
  , _opSrc = Local
  , _spark = 0
  }

defaultOutput :: FishState -> Pure FishOut
defaultOutput _ = FishOut
  { _remoteWritePush = Nothing
  , _localWritePush = Nothing
  , _fetchCmdPush   = Nothing
  , _cmdPop = False
  , _localCmdPop = False
  , _fetchCmdPop = False
  , _sparksPop = False
  }

type ComFish = CPUM FishState FishOut

-- | Synchronous control logic, packaged as a Mealy machine
comFish :: (HiddenClockResetEnable dom)
    => Signal dom CoreId -> Signals dom FishIn -> Signals dom FishOut
comFish = traceFSM "_com_fish" initState defaultOutput step

step :: Pure FishIn -> ComFish ()
step FishIn{..} = use phase >>= \case
  Init -> do
    -- Find next command
    whenM (isNothing <$> use op) nextCmd
    use op >>= \case
      Nothing -> pure ()

      -- Start a local request
      Just (Left addr) -> if numSparks == 0
        -- Send fish onwards
        then when remoteWriteReady $ do
          remoteWritePush .:= Just (MFish (cid, addr) (maxBound - 1))
          finish
        -- Try to find something local
        else when threadsReady $ do
          phase .= Read
          sparksPop .:= True
          -- TODO Do we need to worry about fetching thread requests colliding with sparks?

      -- Start a remote request
      Just (Right (fuel,dst)) ->
        if fuel == 0
          then giveup
          else if numSparks == 0
            -- Send fish onwards
            then when remoteWriteReady $ do
              remoteWritePush .:= Just (MFish dst (fuel - 1))
              finish
            else when threadsReady $ do
              phase .= Read
              sparksPop .:= True

  Read -> case sparks of
    Nothing -> pure ()
    Just (tag,addr) ->
      if tag == Sparked
        then spark .= addr >> dispatch
        else phase .= Init -- Retry!

  Fail -> giveup
  Dispatch -> dispatch
  where
    cid = coreId
    nextCmd
      | isJust localCmd = do
          op .= Just (Left $ fromJust localCmd)
          opSrc .= Local
      | isJust fetchCmd = do
          op .= Just (Right $ fromJust fetchCmd)
          opSrc .= Retry
      | isJust cmd = do
          op .= Just (Right $ fromJust cmd)
          opSrc .= Remote
      | otherwise = pure ()

    dispatch = do
      phase .= Dispatch
      addr <- use spark
      use op >>= \case
        Just (Left _) -> when localWriteReady $ do
          localWritePush .:= Just (CMLocalSpark addr)
          finish
        Just (Right (_,dst)) -> when fetchReady $ do
            fetchCmdPush .:= Just (FetchCmd 0 (cid,addr) dst)
            finish
        _ -> errorX "Unexpected fish op"

    doPop = use opSrc >>= \case
      Local -> localCmdPop .:= True
      Retry -> fetchCmdPop .:= True
      Remote -> cmdPop .:= True

    finish = do
      phase .= Init
      op .= Nothing
      doPop

    giveup = do
      phase .= Fail
      use op >>= \case
        Just (Left _) -> when localWriteReady $ do
          localWritePush .:= Just CMFizzled
        Just (Right (_,dst)) -> when remoteWriteReady $ do
          remoteWritePush .:= Just (MFizzled dst)
        _ -> errorX "Unexpected fish op"
      finish
