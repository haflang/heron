{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-} -- For unused generated lenses

module Heron.Schedule.RemoteWrite where

import           Barbies.TH
import           Clash.Prelude
import           Control.Lens         hiding (Index, assign, at, both, imap, op,
                                       (:>))
import           Control.Monad
import           Data.Maybe
import           Heron.Core.Heap      ()
import           Heron.Schedule.Types
import           Heron.Template
import           RetroClash.Barbies
import           RetroClash.CPU       hiding (update)

declareBareB [d|
  data RemoteWriteIn = RemoteWriteIn
    { writePortsReady :: Vec 4 Bool
    , fishDir     :: Workload
    , localWReady :: Bool
    , remoteRead :: Maybe ComMsg
    , localRead  :: Maybe ComMsg
    , context    :: Maybe ComMsg
    , fish       :: Maybe ComMsg
    , fetch      :: Maybe ComMsg
    , refCount   :: Maybe ComMsg
    , coreId     :: CoreId
    } |]
deriving instance Generic (Pure RemoteWriteIn)
deriving instance NFDataX (Pure RemoteWriteIn)
deriving instance BitPack (Pure RemoteWriteIn)
deriving instance Show    (Pure RemoteWriteIn)
deriving instance ShowX   (Pure RemoteWriteIn)

type RemoteWriteState = ()

declareBareB [d|
  data RemoteWriteOut = RemoteWriteOut
    { _writePortsPush :: Vec 4 (Maybe ComMsg)
    , _localWPush     :: Maybe ComMutRequest
    , _remoteReadPop :: Bool
    , _localReadPop  :: Bool
    , _contextPop :: Bool
    , _fishPop :: Bool
    , _fetchPop :: Bool
    , _refCountPop :: Bool
    } |]
makeLenses ''RemoteWriteOut
deriving instance Generic (Pure RemoteWriteOut)
deriving instance NFDataX (Pure RemoteWriteOut)
deriving instance BitPack (Pure RemoteWriteOut)
deriving instance Show    (Pure RemoteWriteOut)
deriving instance ShowX   (Pure RemoteWriteOut)

initState :: RemoteWriteState
initState = ()

defaultOutput :: RemoteWriteState -> Pure RemoteWriteOut
defaultOutput _ = RemoteWriteOut
  { _writePortsPush = repeat Nothing
  , _localWPush     = Nothing
  , _remoteReadPop  = False
  , _localReadPop   = False
  , _contextPop     = False
  , _fishPop        = False
  , _fetchPop       = False
  , _refCountPop    = False
  }

type ComRemoteWrite = CPUM RemoteWriteState RemoteWriteOut

-- | Synchronous control logic, packaged as a Mealy machine
comRemoteWrite :: (HiddenClockResetEnable dom)
    => Signals dom RemoteWriteIn -> Signals dom RemoteWriteOut
comRemoteWrite = mealyCPU initState defaultOutput step

step :: Pure RemoteWriteIn -> ComRemoteWrite ()
step RemoteWriteIn{..} = case mpacket of
  Nothing -> pure ()
  Just (pkt, pop) -> do
    let cid = coreId
    case txPort cid fishDir pkt of
      Just p -> write pop pkt p
      Nothing -> -- Only fizzled sparks fail routing
        if cid == fst fishsrc
          then localWrite CMFizzled
          else write pop (MFizzled fishsrc) (route cid fishsrc)

  where
    Just (MFish fishsrc _) = fish
    opts =
           fmap (,localReadPop  .:= True) localRead  :>
           fmap (,remoteReadPop .:= True) remoteRead :>
           fmap (,fishPop       .:= True) fish       :>
           fmap (,contextPop    .:= True) context    :>
           fmap (,fetchPop      .:= True) fetch      :>
           fmap (,refCountPop   .:= True) refCount   :>
           Nil
    mpacket = fold (<|>) opts
    -- TODO We should think about how we give priortiy a bit more formally

    localWrite m = do
      when localWReady $ do
        localWPush .:= Just m
        fishPop .:= True

    write callback m p = do
      let i = portIndex p
      when (writePortsReady !! i) $ do
        writePortsPush .:= portsPush p m
        callback

txPort :: CoreId -> Workload -> ComMsg -> Maybe PortId
txPort cid w (MFish _ fuel)
  | 0 == fuel || wkLoad w == 0 = Nothing
  | otherwise = Just $ route cid (wkId w, 0)
txPort cid _ m = Just . route cid . fromJust $ routeAddr m

route :: CoreId -> GlobalAddr -> PortId
route (x,y) ((x',y'),_)
  | x==x' && y==y' = errorX "Routing packet from node to itself"
  | otherwise = fromMaybe lr ud
  where
    ud = case compare y y' of
           GT -> Just PUp
           LT -> Just PDown
           EQ -> Nothing
    lr = case compare x x' of
           GT -> PLeft
           LT -> PRight
           EQ -> PDown -- Dummy value, should never be used. errorX $ "Routing packet from a node to itself"

routeAddr :: ComMsg -> Maybe GlobalAddr
routeAddr (MFish          _ _) = Nothing
routeAddr (MFizzled   dst    ) = Just dst
routeAddr (MFetch     dst _ _) = Just dst
routeAddr (MPut     _ dst _ _) = Just dst
routeAddr (MSuspend   dst _  ) = Just dst
routeAddr (MUnblock   dst    ) = Just dst
routeAddr (MCollected dst    ) = Just dst
routeAddr (MFail      _   _  ) = Just ((0,0),0)
