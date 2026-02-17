{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-} -- For unused generated lenses

module Heron.Schedule.LocalWrite where

import           Barbies.TH
import           Clash.Prelude        hiding (read)
import           Control.Lens         hiding (Index, assign, at, both, imap, op,
                                       (:>))
import           Data.Maybe           (isJust)
import           Heron.Core.Heap      ()
import           Heron.Schedule.Types
import           RetroClash.Barbies
import           RetroClash.CPU       hiding (update)

declareBareB [d|
  data LocalWriteIn = LocalWriteIn
    { mutReady   :: Bool
    , putCmd     :: Maybe ComMutRequest
    , fizzleCmd  :: Maybe ComMutRequest
    , fishCmd    :: Maybe ComMutRequest
    , contextCmd :: Maybe ComMutRequest
    , remoteWCmd :: Maybe ComMutRequest
    } |]
deriving instance Generic (Pure LocalWriteIn)
deriving instance NFDataX (Pure LocalWriteIn)
deriving instance BitPack (Pure LocalWriteIn)
deriving instance Show    (Pure LocalWriteIn)
deriving instance ShowX   (Pure LocalWriteIn)

type LocalWriteState = ()

declareBareB [d|
  data LocalWriteOut = LocalWriteOut
    { _mutPush :: Maybe ComMutRequest
    , _putPop     :: Bool
    , _fizzlePop  :: Bool
    , _fishPop    :: Bool
    , _contextPop :: Bool
    , _remoteWPop :: Bool
    } |]
makeLenses ''LocalWriteOut
deriving instance Generic (Pure LocalWriteOut)
deriving instance NFDataX (Pure LocalWriteOut)
deriving instance BitPack (Pure LocalWriteOut)
deriving instance Show    (Pure LocalWriteOut)
deriving instance ShowX   (Pure LocalWriteOut)

initState :: LocalWriteState
initState = ()

defaultOutput :: LocalWriteState -> Pure LocalWriteOut
defaultOutput _ = LocalWriteOut
  { _mutPush = Nothing
  , _putPop     = False
  , _fizzlePop  = False
  , _fishPop    = False
  , _contextPop = False
  , _remoteWPop = False
  }

type ComLocalWrite = CPUM LocalWriteState LocalWriteOut

-- | Synchronous control logic, packaged as a Mealy machine
comLocalWrite :: (HiddenClockResetEnable dom)
    => Signals dom LocalWriteIn -> Signals dom LocalWriteOut
comLocalWrite = mealyCPU initState defaultOutput step

step :: Pure LocalWriteIn -> ComLocalWrite ()
step LocalWriteIn{..}
  | not mutReady = pure ()
  | isJust remoteWCmd = remoteWPop .:= True >> mutPush .:= remoteWCmd
  | isJust putCmd     = putPop     .:= True >> mutPush .:= putCmd
  | isJust fizzleCmd  = fizzlePop  .:= True >> mutPush .:= fizzleCmd
  | isJust contextCmd = contextPop .:= True >> mutPush .:= contextCmd
  | isJust fishCmd    = fishPop    .:= True >> mutPush .:= fishCmd
  | otherwise = pure ()
  -- TODO Might want to try round robin?
