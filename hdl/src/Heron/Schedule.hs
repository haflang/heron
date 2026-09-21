{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-} -- For unused generated lenses
{-| The scheduler logic for a single core.

responsible for communications to neighbours, including distribution of data and
sparks.
-}
module Heron.Schedule where

import           Barbies.TH
import           Clash.Prelude                hiding (read)
import           Control.Lens                 hiding (Index, assign, at, both,
                                               imap, op, (:>))
import           Data.Maybe                   (fromMaybe, isJust)
import           GHC.Stack                    (HasCallStack)
import           RetroClash.Barbies

import           Heron.Core.Fifo
import           Heron.Core.Heap              ()
import           Heron.Error
import           Heron.Parameters
import qualified Heron.Primitives.DualPortRam as DP
import           Heron.Schedule.Context       (ContextIn (..), ContextOut (..),
                                               comContext)
import           Heron.Schedule.Fetch         (FetchIn (..), FetchOut (..),
                                               comFetch)
import           Heron.Schedule.Fish          (FishIn (..), FishOut (..),
                                               comFish)
import           Heron.Schedule.LocalRead     (LocalReadIn (..),
                                               LocalReadOut (..), comLocalRead)
import           Heron.Schedule.LocalWrite    (LocalWriteIn (..),
                                               LocalWriteOut (..),
                                               comLocalWrite)
import           Heron.Schedule.Put           (PutIn (..), PutOut (..), comPut)
import           Heron.Schedule.RefCounts     (RefCountIn (..),
                                               RefCountOut (..), comRefCount)
import           Heron.Schedule.RemoteRead    (RemoteReadIn (..),
                                               RemoteReadOut (..),
                                               comRemoteRead)
import           Heron.Schedule.RemoteWrite   (RemoteWriteIn (..),
                                               RemoteWriteOut (..),
                                               comRemoteWrite)
import           Heron.Schedule.Threads       (ThreadIn (..), ThreadOut (..),
                                               comThread)
import           Heron.Schedule.Types
import           Heron.Template

-- | Inputs to the Com subsystem
declareBareB [d|
  data ComIn = ComIn
    { heapComIn :: Maybe HeapNode
    , heapComReady :: Bool -- Be careful, this comes unregistered from the mutator!
    , readPorts  :: Vec 4 ReadPort
    , writePortsReady :: Vec 4 Bool
    , readWorkloads :: Vec 4 Workload
    , mutComRequest :: Maybe MutComRequest
    , mutThreadCmd :: Maybe ThreadCmd
    , updRequest :: Maybe GlobalAddr
    , mutUpdate :: Maybe HeapAddr
    , gcRCRequest :: Maybe RefCountCmd
    , gcThreadCmd :: Maybe ThreadCmd
    , mutRCRequest :: Maybe RefCountCmd
    , fetchUpdateReady :: Bool
    , errPush :: Maybe Err
    , priorityReset :: Bool
    } |]
deriving instance Generic (Pure ComIn)
deriving instance NFDataX (Pure ComIn)
deriving instance Show    (Pure ComIn)
deriving instance ShowX   (Pure ComIn)

-- | Outputs from the collector subsystem
declareBareB [d|
  data ComOut = ComOut
    { _heapComOut :: RamOp HeapSize HeapNode
    , _readPortsPop :: Vec 4 Bool
    , _writeWorkloads :: Vec 4 Workload
    , _writePortsPush :: Vec 4 (Maybe ComMsg)
    , _comMutRequest :: Maybe ComMutRequest
    , _comMutReady :: Bool
    , _reqLatency :: Unsigned 32
    , _readyPoolHead :: Maybe TSOAddr
    , _numSparks :: HeapAddr
    , _gcNextRef :: Maybe (Maybe (HeapAddr, RefCountNode))
    , _gcRCReady :: Bool
    , _gcThreadRet :: Maybe ThreadResult
    , _gcThreadReady :: Bool
    , _mutRCReady :: Bool
    , _fetchUpdatePush :: Maybe HeapNode
    } |]
makeLenses ''ComOut
deriving instance Generic (Pure ComOut)
deriving instance NFDataX (Pure ComOut)
deriving instance Show    (Pure ComOut)
deriving instance ShowX   (Pure ComOut)

-- | Synchronous control logic, packaged as a Mealy machine
com :: forall dom
     . ( HasCallStack
       , HiddenClockResetEnable dom)
    => Signal dom CoreId -> Signals dom ComIn -> Signals dom ComOut
com cid ComIn{..} = ComOut{..}
  where

    (thMemComOutT, thMemAuxOutT) = DP.dpRam @HeapSize @dom @ThreadTag        DP.BlockRam (fst <$> thMemCom) (fst <$> thMemAux)
    (thMemComOutL, thMemAuxOutL) = DP.dpRam @HeapSize @dom @(Maybe HeapAddr) DP.BlockRam (snd <$> thMemCom) (snd <$> thMemAux)
    thMemComOut = (,) <$> thMemComOutT <*> thMemComOutL
    thMemAuxOut = (,) <$> thMemAuxOutT <*> thMemAuxOutL

    (comRCMemOut, gcRCMemOut) = DP.dpRam @HeapSize @dom @RefCountNode DP.UltraRam rcMemCom rcMemGc

    _gcRCReady = notAlmostFull <$> gc2rc
    _gcThreadReady = notAlmostFull <$> gc2th
    _mutRCReady = notAlmostFull <$> m2rc

    -- Calculate workloads
    bestFishOffer = liftA3 calcFishRoute readWorkloads cid _numSparks
    _writeWorkloads = autoReg (repeat $ Workload (0,0) 0 0) (repeat <$> bestFishOffer)

    -- FSM instantiations
    RefCountOut _gcNextRef rcMemGc rcMemCom fe2rcPop m2rcPop rr2rcPop gc2rcPop rc2rwPush =
      comRefCount @dom cid refcountIn
    refcountIn = RefCountIn
      (_next <$> fe2rc)
      (_next <$> m2rc)
      (_next <$> rr2rc)
      (_next <$> gc2rc)
      comRCMemOut
      gcRCMemOut
      (notAlmostFull <$> rc2rw)

    ThreadOut thOutCom _gcThreadRet _readyPoolHead nextSpark thMemCom thMemAux _numSparks fe2thPop fi2thPop c2thPop gc2thPop =
      comThread @dom cid threadsIn
    threadsIn = ThreadIn
      (_next <$> fe2th)
      (_next <$> fi2th)
      (_next <$> c2th)
      (register Nothing mutThreadCmd)
      (_next <$> gc2th)
      thMemComOut
      thMemAuxOut
    fi2thPush = mux sparkPoolPop (pure $ Just PopSpark) (pure Nothing)
    fe2thPush = fmap FizzleSpark <$> feFizzle

    LocalReadOut mut2lrPop upd2lrPop errPop lr2rwPush lr2fiPush lr2cPush =
      comLocalRead @dom localReadIn
    localReadIn = LocalReadIn
        (_next <$> mut2lr)
        (_next <$> upd2lr)
        (_next <$> err   )
        (notAlmostFull <$> lr2rw)
        (notAlmostFull <$> lr2fi)
        (notAlmostFull <$> lr2c )
        cid

    RemoteReadOut _readPortsPop rr2rwPush rr2pPush rr2lwPush rr2fePush rr2fiPush rr2cPush rr2rcPush =
      comRemoteRead @dom remoteReadIn
    remoteReadIn = RemoteReadIn
        readPorts
        ((==0) . _size <$> rr2rw)
        ((==0) . _size <$> rr2p)
        ((==0) . _size <$> rr2lw)
        ((==0) . _size <$> rr2fe)
        ((==0) . _size <$> rr2fi)
        ((==0) . _size <$> rr2c)
        (notAlmostFull <$> rr2rc)
        cid
        priorityReset

    PutOut p2lwPush rr2pPop =
      comPut @dom $ PutIn
        (_next <$> rr2p)
        (notAlmostFull <$> p2lw)
        cid

    FetchOut fe2rwPush fe2fiPush rr2fePop fi2fePop heapOutFetch clearHeapFetch feFizzle fe2rcPush _fetchUpdatePush =
      comFetch @dom cid fetchIn
    fetchIn = FetchIn
        -- TODO Trying to eliminate race conditions between putting and fetching by prioritising puts... Maybe this is silly.
        -- (mux (isJust . _next <$> rr2p) (pure Nothing) (_next <$> rr2fe))
        -- (mux (isJust . _next <$> rr2p) (pure Nothing) (_next <$> fi2fe))
        (_next <$> rr2fe)
        (_next <$> fi2fe)
        (notAlmostFull <$> fe2rw)
        (notAlmostFull <$> fe2fi)
        (notAlmostFull <$> fe2th)
        (notAlmostFull <$> fe2rc)
        heapInFetch
        mutUpdate
        fetchUpdateReady
        cid

    FishOut fi2rwPush fi2lwPush fi2fePush rr2fiPop lr2fiPop fe2fiPop sparkPoolPop =
      comFish @dom cid fishIn
    fishIn = FishIn
        (_next <$> rr2fi)
        (_next <$> lr2fi)
        (_next <$> fe2fi)
        (notAlmostFull <$> fi2rw)
        (notAlmostFull <$> fi2lw)
        (notAlmostFull <$> fi2fe)
        (notAlmostFull <$> fi2th)
        _numSparks
        nextSpark
        mutUpdate
        cid

    ContextOut c2rwPush c2lwPush rr2cPop lr2cPop heapOutContext clearHeapContext c2thPush =
      comContext @dom cid contextIn
    contextIn = ContextIn
        (_next <$> rr2c)
        (_next <$> lr2c)
        (notAlmostFull <$> c2rw)
        (notAlmostFull <$> c2lw)
        (notAlmostFull <$> c2th)
        heapInContext
        thOutCom
        cid

    RemoteWriteOut _writePortsPush rw2lwPush rr2rwPop lr2rwPop c2rwPop fi2rwPop fe2rwPop rc2rwPop =
      comRemoteWrite @dom remoteWriteIn
    remoteWriteIn = RemoteWriteIn
        writePortsReady
        (autoReg (Workload (0,0) 0 0) bestFishOffer)
        (notAlmostFull <$> rw2lw)
        (_next <$> rr2rw)
        (_next <$> lr2rw)
        (_next <$> c2rw)
        (_next <$> fi2rw)
        (_next <$> fe2rw)
        (_next <$> rc2rw)
        cid

    LocalWriteOut _comMutRequest p2lwPop rr2lwPop fi2lwPop c2lwPop rw2lwPop =
      comLocalWrite @dom localWriteIn
    localWriteIn = LocalWriteIn
        (pure True) -- FIXME Should the output be buffered too? Probably.
        (_next <$> p2lw)
        (_next <$> rr2lw)
        (_next <$> fi2lw)
        (_next <$> c2lw)
        (_next <$> rw2lw)

    -- Arbitrate heap access
    -- TODO Is it sensible to always prioritise fetches? Maybe round-robin instead?
    -- TODO I think this heap arbitration only ever uses half of the available cycles... maybe this isn't so bad though --- it gives the GC a turn too.
    _reqLatency = 0 -- TODO

    checkConflict y (RamWrite x _) = y == Just x
    checkConflict _ _              = False
    -- Latch these puppies and add a clear signal
    heapInFetch   = mux (register False clearHeapFetch) (pure Nothing) $
                    mux heapFetchDone heapComIn (register Nothing heapInFetch)
    heapInContext = mux (register False clearHeapContext) (pure Nothing) $
                    mux heapContextDone heapComIn (register Nothing heapInContext)
    heapFetchDone   = register False $ resultPop Fe
    heapContextDone = register False $ resultPop C

    resultPop a   = ((a ==) <$> heapArb') .&&. heapComReady
    conflictPop a = ((a ==) <$> heapArb') .&&. liftA2 checkConflict  mutUpdate heapOp
    heapPopFetch   = resultPop Fe .||. conflictPop Fe
    heapPopContext = resultPop C  .||. conflictPop C

    _heapComOut = mux (liftA2 checkConflict mutUpdate heapOp) (pure RamNoOp) heapOp
    heapOp = liftA2 (\a b -> fromMaybe RamNoOp $ _next a <|> _next b) fe2heap c2heap
    -- heapArb  = register Fe heapArb'
    heapArb' = fmap (\a -> if a then Fe else C)
                (isJust . _next <$> fe2heap)

    -- Buffers between all units
    zero :: Unsigned (CLog 2 4) = 0
    err   = newFifo zero bufRam (bundle (errPush, errPop))
    lr2rw = newFifo zero bufRam (bundle (lr2rwPush, lr2rwPop))
    lr2fi = newFifo zero bufRam (bundle (lr2fiPush, lr2fiPop))
    lr2c  = newFifo zero bufRam (bundle (lr2cPush, lr2cPop))
    rr2rw = newFifo zero bufRam (bundle (rr2rwPush, rr2rwPop))
    rr2p  = newFifo zero bufRam (bundle (rr2pPush, rr2pPop))
    rr2fi = newFifo zero bufRam (bundle (rr2fiPush, rr2fiPop))
    rr2fe = newFifo zero bufRam (bundle (rr2fePush, rr2fePop))
    rr2c  = newFifo zero bufRam (bundle (rr2cPush, rr2cPop))
    fi2fe = newFifo zero bufRam (bundle (fi2fePush, fi2fePop))
    p2lw  = newFifo zero bufRam (bundle (p2lwPush, p2lwPop))
    rr2lw = newFifo zero bufRam (bundle (rr2lwPush, rr2lwPop))
    fi2lw = newFifo zero bufRam (bundle (fi2lwPush, fi2lwPop))
    rw2lw = newFifo zero bufRam (bundle (rw2lwPush, rw2lwPop))
    c2lw  = newFifo zero bufRam (bundle (c2lwPush, c2lwPop))
    c2rw  = newFifo zero bufRam (bundle (c2rwPush, c2rwPop))
    fi2rw = newFifo zero bufRam (bundle (fi2rwPush, fi2rwPop))
    fe2rw = newFifo zero bufRam (bundle (fe2rwPush, fe2rwPop))
    mut2lr = newFifo zero bufRam (bundle (mutComRequest, mut2lrPop))
    upd2lr = newFifo zero bufRam (bundle (updRequest, upd2lrPop))
    fe2fi = newFifo zero bufRam (bundle (fe2fiPush, fe2fiPop))
    fe2th = newFifo zero bufRam (bundle (fe2thPush, fe2thPop))
    fi2th = newFifo zero bufRam (bundle (fi2thPush, fi2thPop))
    c2th = newFifo zero bufRam (bundle (c2thPush, c2thPop))
    fe2rc = newFifo zero bufRam (bundle (fe2rcPush, fe2rcPop))
    m2rc  = newFifo zero bufRam (bundle (mutRCRequest, m2rcPop))
    rr2rc = newFifo zero bufRam (bundle (rr2rcPush, rr2rcPop))
    rc2rw = newFifo zero bufRam (bundle (rc2rwPush, rc2rwPop))
    gc2rc = newFifo zero bufRam (bundle (gcRCRequest, gc2rcPop))
    gc2th = newFifo zero bufRam (bundle (gcThreadCmd, gc2thPop))
    fe2heap :: Signal dom (FOut (RamOp HeapSize HeapNode) 4) = newFifo zero bufRam (bundle (heapOutFetch, heapPopFetch))
    c2heap  :: Signal dom (FOut (RamOp HeapSize HeapNode) 4) = newFifo zero bufRam (bundle (heapOutContext, heapPopContext))

    _comMutReady = (notAlmostFull <$> mut2lr) .&&. (notAlmostFull <$> upd2lr)

    -- TODO Remember to buffer other inputs/outputs from/to Board

data HeapArb = Fe | Fi | C
  deriving (Generic, NFDataX, Show, ShowX, Eq)

bufRam :: (HiddenClockResetEnable dom, NFDataX a) => Signal dom (Unsigned (CLog 2 4)) -> Signal dom (Maybe (Unsigned (CLog 2 4), Maybe a)) -> Signal dom (Maybe a)
bufRam = blockRam1 ClearOnReset d4 Nothing

-- | Derive the best fishing route we can offer our neighbours. This is the most
-- direct route to the PE with the most sparks. We might want to pipeline this
-- if it causes problems --- logic level of about 1+3*2+5=12...
calcFishRoute
  :: Vec 4 Workload
  -> CoreId
  -> HeapAddr
  -> Workload
calcFishRoute others cid w = incHop $ best this bestRemote
  where
    this = Workload { wkHop = 0, wkLoad = w, wkId = cid }
    incHop wl = wl { wkHop = satAdd SatBound 1 (wkHop wl) }
    bestRemote = shortPath $ fold best others
    shortPath wl
      | minDistance (wkId wl) cid == wkHop wl = wl
      | otherwise = Workload (0,0) 0 0
    best a b = case compare (wkLoad a) (wkLoad b) of
      LT -> b
      GT -> a
      EQ -> if wkHop a <= wkHop b then a else b

-- Needs 5 adders... not great.
minDistance :: CoreId -> CoreId -> Index (1+HopCount)
minDistance (x1,y1) (x2,y2) = hops
  where
    hops :: Index (1+HopCount) = bitCoerce $ resize dx + resize dy
    dx :: Signed (1 + CLog 2 MaxPECols) = abs $
      bitCoerce (resize x1 :: Index (2*MaxPECols)) -
      bitCoerce (resize x2 :: Index (2*MaxPECols))
    dy :: Signed (1 + CLog 2 MaxPERows) = abs $
      bitCoerce (resize y1 :: Index (2*MaxPERows)) -
      bitCoerce (resize y2 :: Index (2*MaxPERows))

{-# NOINLINE com #-}
