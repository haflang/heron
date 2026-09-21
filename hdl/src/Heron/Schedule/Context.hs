{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-} -- For unused generated lenses

{-
-}
module Heron.Schedule.Context where

import           Barbies.TH
import           Clash.Prelude        hiding (read)
import           Control.Lens         hiding (Index, assign, at, both, imap, op,
                                       (:>))
import           Control.Monad
import           Data.Maybe
import           Heron.Core.Heap      ()
import           Heron.Parameters
import           Heron.Schedule.Types
import           Heron.Template
import           Heron.TraceFSM
import           RetroClash.Barbies
import           RetroClash.CPU       hiding (update)


data ContextCmd
  = Suspend TSOAddr (Maybe GlobalAddr)
  | Resume TSOAddr
  deriving (Generic, NFDataX, Show, ShowX, Eq, BitPack)

declareBareB [d|
  data ContextIn = ContextIn
    { cmd    :: Maybe ContextCmd
    , localCmd :: Maybe ContextCmd
    , remoteWriteReady :: Bool
    , localWriteReady :: Bool
    , threadsReady :: Bool
    , heapIn :: Maybe HeapNode
    , threadsDone :: Bool
    , coreId :: CoreId
    } |]
deriving instance Generic (Pure ContextIn)
deriving instance NFDataX (Pure ContextIn)
deriving instance BitPack (Pure ContextIn)
deriving instance Show    (Pure ContextIn)
deriving instance ShowX   (Pure ContextIn)

data Phase
  = Go
  | CheckNextResume TSOAddr
  | Resuming TSOAddr
  | DispatchNextResume CoreId TSOAddr
  deriving (Generic, NFDataX, Show, ShowX, BitPack)

data ContextState = ContextState
  { _phase :: Phase
  , _local :: Bool
  , _op    :: Maybe ContextCmd
  }
  deriving (Generic, NFDataX, Show, ShowX, BitPack, AutoReg)
makeLenses ''ContextState

declareBareB [d|
  data ContextOut = ContextOut
    { _remoteWritePush :: Maybe ComMsg
    , _localWritePush :: Maybe ComMutRequest
    , _cmdPop :: Bool
    , _localCmdPop :: Bool
    , _heapOut :: Maybe (RamOp HeapSize HeapNode)
    , _clearHeap :: Bool
    , _threadCmd :: Maybe ThreadCmd
    } |]
makeLenses ''ContextOut
deriving instance Generic (Pure ContextOut)
deriving instance NFDataX (Pure ContextOut)
deriving instance BitPack (Pure ContextOut)
deriving instance Show    (Pure ContextOut)
deriving instance ShowX   (Pure ContextOut)

initState :: ContextState
initState = ContextState
  { _phase = Go
  , _op = Nothing
  , _local = False
  }

defaultOutput :: ContextState -> Pure ContextOut
defaultOutput _ = ContextOut
  { _remoteWritePush = Nothing
  , _localWritePush = Nothing
  , _cmdPop = False
  , _localCmdPop = False
  , _heapOut = Nothing
  , _clearHeap = False
  , _threadCmd = Nothing
  }

type ComContext = CPUM ContextState ContextOut

-- | Synchronous control logic, packaged as a Mealy machine
comContext :: (HiddenClockResetEnable dom)
    => Signal dom CoreId -> Signals dom ContextIn -> Signals dom ContextOut
comContext = traceFSM "_com_ctxt" initState defaultOutput step

step :: Pure ContextIn -> ComContext ()
step ContextIn{..} = use phase >>= \case
  Go -> do
    -- Grab next cmd
    c <- use op
    when (isNothing c) $
      case fmap (,True) localCmd <|> fmap (,False) cmd of
        Nothing -> pure ()
        Just (cmd',local') -> do
          op .= Just cmd'
          local .= local'

    use op >>= \case
      Nothing -> pure ()
      Just (Suspend tso tailb) -> suspend tso tailb
      Just (Resume  tso) -> when threadsReady $ do
        phase .= Resuming tso
        threadCmd .:= Just (Unblock tso)
  CheckNextResume tso -> case heapIn of
    Nothing -> pure ()
    Just (TSOHead rest _ _) -> clearHeap .:= True >> case rest of
      Nothing -> finish
      Just (cid',t) -> do
        if coreId == cid'
          then do
            op .= Just (Resume t)
            phase .= Go
          else dispatchNext cid' t
    Just _ -> heapOut .:= Just (RamRead tso) -- Looks like TSO hasn't been written yet... try again.
  DispatchNextResume cid' t -> when remoteWriteReady $ do
        remoteWritePush .:= Just (MUnblock (cid',t))
        finish

  Resuming t -> when threadsDone $ do
    phase .= CheckNextResume t
    heapOut .:= Just (RamRead t)
  where

    finish = do
      popOp
      op .= Nothing
      phase .= Go
    popOp = use local >>= \case
      True -> localCmdPop .:= True
      False -> cmdPop .:= True

    suspend tso tailb =
      when (localWriteReady && threadsReady) $ do
        threadCmd .:= Just (PushBlock tso)
        localWritePush .:= Just (CMSuspend tailb)
        finish

    dispatchNext cid' t = do
      phase .= DispatchNextResume cid' t
      when remoteWriteReady $ do
        remoteWritePush .:= Just (MUnblock (cid',t))
        finish
