{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-} -- For unused generated lenses

{-
-}
module Heron.Schedule.LocalRead where

import           Barbies.TH
import           Clash.Prelude          hiding (read)
import           Control.Lens           hiding (Index, assign, at, both, imap,
                                         op, (:>))
import           Control.Monad
import           Data.Maybe             (fromJust, isJust)
import           Heron.Core.Heap        ()
import           Heron.Error
import           Heron.Schedule.Context (ContextCmd (..))
import           Heron.Schedule.Types
import           Heron.Template
import           RetroClash.Barbies
import           RetroClash.CPU         hiding (update)

declareBareB [d|
  data LocalReadIn = LocalReadIn
    { cmd              :: Maybe MutComRequest
    , updCmd           :: Maybe GlobalAddr
    , errCmd           :: Maybe Err
    , remoteWriteReady :: Bool
    , fishReady        :: Bool
    , contextReady     :: Bool
    , coreId           :: CoreId
    } |]
deriving instance Generic (Pure LocalReadIn)
deriving instance NFDataX (Pure LocalReadIn)
deriving instance BitPack (Pure LocalReadIn)
deriving instance Show    (Pure LocalReadIn)
deriving instance ShowX   (Pure LocalReadIn)

type LocalReadState = ()

declareBareB [d|
  data LocalReadOut = LocalReadOut
    { _cmdPop :: Bool
    , _updCmdPop :: Bool
    , _errCmdPop :: Bool
    , _remoteWritePush :: Maybe ComMsg
    , _fishPush        :: Maybe HeapAddr
    , _contextPush     :: Maybe ContextCmd
    } |]
makeLenses ''LocalReadOut
deriving instance Generic (Pure LocalReadOut)
deriving instance NFDataX (Pure LocalReadOut)
deriving instance BitPack (Pure LocalReadOut)
deriving instance Show    (Pure LocalReadOut)
deriving instance ShowX   (Pure LocalReadOut)

initState :: LocalReadState
initState = ()

defaultOutput :: LocalReadState -> Pure LocalReadOut
defaultOutput _ = LocalReadOut
  { _cmdPop = False
  , _updCmdPop = False
  , _errCmdPop = False
  , _remoteWritePush = Nothing
  , _fishPush        = Nothing
  , _contextPush     = Nothing
  }

type ComLocalRead = CPUM LocalReadState LocalReadOut

-- | Synchronous control logic, packaged as a Mealy machine
comLocalRead :: (HiddenClockResetEnable dom)
    => Signals dom LocalReadIn -> Signals dom LocalReadOut
comLocalRead = mealyCPU initState defaultOutput step

step :: Pure LocalReadIn -> ComLocalRead ()
step LocalReadIn{..}
  | isJust errCmd = err $ fromJust errCmd
  | isJust updCmd = upd $ fromJust updCmd
  | isJust cmd    = mut $ fromJust cmd
  | otherwise     = pure ()
  where
    cid = coreId

    err e
      | cid == (0,0) = errCmdPop .:= True -- Ignore errors emitted by the master
      | otherwise = when remoteWriteReady $ do
          errCmdPop .:= True
          remoteWritePush .:= Just (MFail cid e)

    upd (cid',b)
      | cid==cid'
      = when contextReady $ do
          updCmdPop .:= True
          contextPush .:= Just (Resume b)
      | otherwise
      = when remoteWriteReady $ do
          updCmdPop .:= True
          remoteWritePush .:= Just (MUnblock (cid',b))

    mut c =
      case c of
        MCFetch tso dst addr -> when remoteWriteReady $ do
          cmdPop .:= True
          remoteWritePush .:= Just (MFetch dst tso (cid,addr))
        MCFish addr          -> when fishReady $ do
          cmdPop .:= True
          fishPush .:= Just addr
        MCLocalSuspend tso btail -> when contextReady $ do
          cmdPop .:= True
          contextPush .:= Just (Suspend tso btail)
        MCFail e -> when remoteWriteReady $ do
          cmdPop .:= True
          remoteWritePush .:= Just (MFail cid e)
        MCCollected _        -> error "Global GC not implemented"

-- TODO Are we right to prioritise mut requests over unblocking?
