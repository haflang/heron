{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Heron.Sim where

import           Clash.Prelude              hiding (read)
import           Control.Monad              (zipWithM_)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Char                  (chr, ord)
import           Data.Either                (fromRight)
import qualified Data.List                  as L
import           Data.List.Extra            (chunksOf)
import           Data.Maybe                 (fromJust)
import qualified Flite.TemplateSyntax       as TS
import           Heron.Board
import           Heron.Core.Collector       (GCIn, GCOut, GCState)
import           Heron.Core.Core
import           Heron.Core.Types
import           Heron.Encode               (encProg)
import           Heron.Error                (explainErr)
import           Heron.Log
import           Heron.Parameters
import           Heron.PE
import           Heron.Schedule.Context     (ContextIn, ContextOut,
                                             ContextState)
import           Heron.Schedule.Fetch       (FetchIn, FetchOut, FetchState)
import           Heron.Schedule.Fish        (FishIn, FishOut, FishState)
import           Heron.Schedule.RefCounts   (RefCountIn, RefCountOut,
                                             RefCountState)
import           Heron.Schedule.Threads     (ThreadIn, ThreadOut, ThreadState)
import           Heron.Schedule.Types       (Workload (..))
import           Heron.Template
import           Heron.VCD
import qualified Prelude                    as P
import           RetroClash.Barbies

simDropCycles :: TS.Prog -> Int
simDropCycles _p = 3 + heapSize -- (heapSize `div` 2)
  where
    heapSize = snatToNum (SNat @HeapSize)

simInputs :: TS.Prog -> [Pure PEIn]
simInputs p = L.concat
  [ L.replicate 11 defIn -- Waiting for reset to finish
  , [defIn]             -- One cycle to  account for topEntity's extra input register
  , L.zipWith (\a t -> defIn { peCode = RamWrite a t }) [0..] prog
  -- ^ Write each template
  -- , L.replicate (heapSize `div` 2 - L.length prog) defIn
  , L.replicate (heapSize - L.length prog - 1) defIn
  -- ^ Pause until GC has initialised
  , [defIn{ peGo = Just initAddr, peRelease = True }]
  -- ^ Start reduction
  , L.repeat $ defIn { peRelease = True }
  -- ^ Wait
  ]
  where
    (initAddr, prog) = encProg p
    defIn :: Pure PEIn
    defIn = PEIn
      { peGo = Nothing
      , peRelease = False
      , peCode = RamNoOp
      , peGcThreshold = defGcThreshold
      , pePortsRx = repeat (PortRx Nothing False $ Workload (0,0) 0 0)
      , peId = (0,0)
      }
    heapSize = snatToNum (SNat @HeapSize)

simReport :: [Pure PEOut] -> IO (Int, CPUStats, Atom)
simReport simOut = do
  writeFile "/tmp/heron_sim.log" ""
  let doneAt = checkSamples $ L.zip [0::Integer ..] simOut
  case doneAt of
    Nothing  -> do
      error "Core still running... Aborting."
    Just (idx, final) -> do
      putStrLn $ L.concat ["Finished after ", show (idx-1), " cycles: "]
      let ans   = fromRight (unpack 0) . fromJust $ _peResult final
      pure (idx - 1, _peStats final, ans)
  where
    checkSamples [] = Nothing
    checkSamples ((i,s):ss) = -- do
      -- appendFile "/tmp/heron_sim.log" ("Cycle " L.++ show i L.++ "===================")
      -- appendFile "/tmp/heron_sim.log" (showState $ snd s)
      case _peResult s of
        Nothing -> checkSamples ss
        Just r  -> case r of
          Left err -> errorX $ unwords ["Returned an error:", explainErr err]
          Right _  -> Just (fromIntegral i, s)

simDUT
  :: forall dom
   . KnownDomain dom
  => Int
  -- ^ Bound on cycle count before termination (pass `maxBound` for maximumg)
  -> (Signals dom PEIn -> Signals dom PEOut)
  -- ^ Design under test
  -> TS.Prog
  -- ^ Input program as F-lite templates
  -> IO [Pure PEOut]
  -- ^ Output samples
simDUT limit dut p =
  let inps = unbundle . fromList $ simInputs p
      hw   = bundle $ dut inps
  in dumpVCD (0,0) hw [] >> -- Without this call, we get an error "Already
                             -- tracing a signal with the name". See
                             -- https://github.com/clash-lang/clash-compiler/issues/1270
     pure (L.take limit $
           L.drop (simDropCycles p) $
           sample hw)

simVcdDUT
  :: forall a dom
   . (TraceState a, KnownDomain dom)
  => Int
  -- ^ Bound on cycle count before termination (pass `maxBound` for maximumg)
  -> (Signals dom PEIn -> Signals dom PEOut)
  -- ^ Design under test
  -> TS.Prog
  -- ^ Input program as F-lite templates
  -> IO [a]
simVcdDUT limit dut p = do
  dumpSignals (simDropCycles p,simDropCycles p+limit) hw
  where
    inps = unbundle . fromList $ simInputs p
    hw = bundle $ dut inps

-- | Simulate `heronSingleCore` at a Haskell-level
sim :: Int
    -- ^ Bound on cycle count before termination (pass `maxBound` for maximumg)
    -> TS.Prog
    -- ^ Input program as F-lite templates
    -> IO (Int, CPUStats, Atom)
    -- ^ (Simulated cycle count, stats, return value)
sim limit p = simDUT limit dut p >>= simReport
  where dut = exposeClockResetEnable @DomIn heronSingleCore
                clockGen resetGen enableGen

-- | Simulate `heronTriCore` at a Haskell-level
simMesh
    :: (KnownNat rows, KnownNat cols, KnownNat slrs)
    => SNat (rows+1)
    -> SNat (cols+1)
    -> SNat (slrs+1)
    -> Int
    -> TS.Prog
    -- ^ Input program as F-lite templates
    -> IO (Int, CPUStats, Atom)
    -- ^ (Simulated cycle count, stats, return value)
simMesh rows cols slrs limit p = simDUT limit dut p >>= simReport
  where
    dut = exposeClockResetEnable (heronMesh cols rows slrs)
            clockGen resetGen enableGen

-- | Generate VCD trace for `heronSingleCore` simulation
trace :: Int
    -- ^ Bound on cycle count before termination (pass `maxBound` for maximum)
    -> TS.Prog
    -- ^ Input program as F-lite templates
    -> IO ()
trace limit p = do
  _ <- simVcdDUT @PETrace limit dut p
  pure ()
  where
    dut = exposeClockResetEnable @DomIn heronSingleCore
            clockGen resetGen enableGen

-- | Generate VCD trace for `heronTriCore` simulation
traceMesh
  :: forall rows cols slrs
   . (KnownNat rows, KnownNat cols, KnownNat slrs)
  => SNat (rows+1)
  -> SNat (cols+1)
  -> SNat (slrs+1)
  -> Int
  -- ^ Bound on cycle count before termination (pass `maxBound` for maximum)
  -> TS.Prog
  -- ^ Input program as F-lite templates
  -> IO ()
traceMesh rows cols slrs limit p =
  simVcdDUT @(MeshTrace ((slrs+1)*(rows+1)) (cols+1)) limit dut p >>=
  textLog
  where
    dut = withClockResetEnable clockGen resetGen enableGen
            (heronMesh cols rows slrs)

defGcThreshold :: HeapAddr
defGcThreshold = 250 {- perc d94 -- d47
  where
    perc x = snatToNum $
             divSNat (mulSNat x (SNat @(HeapSize `Div` 4)))
                     d100
-}

-- VCD Parse instances

data PETrace = PETrace
  { mutator     :: !(Pure CPUIn     , CPUState     , Pure CPUOut     )
  , gc          :: !(Pure GCIn      , GCState      , Pure GCOut      )
  , comContext  :: !(Pure ContextIn , ContextState , Pure ContextOut )
  , comFetch    :: !(Pure FetchIn   , FetchState   , Pure FetchOut   )
  , comFish     :: !(Pure FishIn    , FishState    , Pure FishOut    )
  , comRefCount :: !(Pure RefCountIn, RefCountState, Pure RefCountOut)
  , comThread   :: !(Pure ThreadIn  , ThreadState  , Pure ThreadOut  )
  }
  deriving (Show, Generic, ShowX)

instance TraceState PETrace where
  keys =
    [ "mutator"
    , "gc"
    , "com_ctxt"
    , "com_fetch"
    , "com_fish"
    , "com_rc"
    , "com_thread"
    ]
  recreate [a,b,c,d,e,f,g] = PETrace
    (unpackBitStr a)
    (unpackBitStr b)
    (unpackBitStr c)
    (unpackBitStr d)
    (unpackBitStr e)
    (unpackBitStr f)
    (unpackBitStr g)
  recreate _ = error "Unexpected number of args to recreate"
  recreateBV [a,b,c,d,e,f,g] = PETrace
    (unpackMask a)
    (unpackMask b)
    (unpackMask c)
    (unpackMask d)
    (unpackMask e)
    (unpackMask f)
    (unpackMask g)
  recreateBV _ = error "Unexpected number of args to recreate"

newtype MeshTrace rows cols
  = MeshTrace (Vec cols (Vec rows PETrace))
  deriving (Show, Generic, ShowX)

instance (KnownNat rows, KnownNat cols) => TraceState (MeshTrace rows cols) where
  keys = P.concat
    [ P.map (\k -> P.concat ["Core_",show (x,y),"_",k]) (keys @PETrace)
    | x <- [0..n-1]
    , y <- [0..m-1]
    ]
    where
      n = snatToNum $ SNat @cols :: Int
      m = snatToNum $ SNat @rows :: Int
  recreate xs = MeshTrace mesh
    where
      m = snatToNum $ SNat @rows :: Int
      peLen = P.length (keys @PETrace)

      ys :: [PETrace]
      ys = P.map recreate $ chunksOf peLen xs

      mesh    = imap col $ repeat @cols ()
      col x _ = imap (\y _ -> ys P.!! coord x y) $ repeat @rows ()
      coord x y = fromIntegral x * m + fromIntegral y
  recreateBV xs = MeshTrace mesh
    where
      m = snatToNum $ SNat @rows :: Int
      peLen = P.length (keys @PETrace)

      ys :: [PETrace]
      ys = P.map recreateBV $ chunksOf peLen xs

      mesh    = imap col $ repeat @cols ()
      col x _ = imap (\y _ -> ys P.!! coord x y) $ repeat @rows ()
      coord x y = fromIntegral x * m + fromIntegral y

showPETrace :: PETrace -> String
showPETrace (PETrace mut g c fe _fi rc th) = L.unlines $ L.map (prefix L.++)
  [ "Stk top   : " L.++ showX (read (vStkIn $ ins mut))
  , "Heap Ctrls: " L.++ showX (_heapOut $ outs mut)
  , "Result    : " L.++ showX (snd $ _result $ outs mut)
  , "Stats     : " L.++ showX (fst $ _result $ outs mut)
  , "AStk top  : " L.++ showX (read (aStkIn  $ ins mut))
  , "Outs      : " L.++ showX (outs mut)
  , "Ins       : " L.++ showX (ins  mut)
  , "State     : " L.++ showX (sts  mut)
  , "GC Ins    : " L.++ showX (ins  g  )
  , "GC Outs   : " L.++ showX (outs g  )
  , "GC State  : " L.++ showX (sts  g  )
  , "Com Fetch Ins    : " L.++ showX (ins  fe )
  , "Com Fetch Outs   : " L.++ showX (outs fe )
  , "Com Fetch State  : " L.++ showX (sts  fe )
  , "Com Thread Ins    : " L.++ showX (ins  th)
  , "Com Thread Outs   : " L.++ showX (outs th)
  , "Com Thread State  : " L.++ showX (sts  th)
  , "Com RC Ins    : " L.++ showX (ins  rc)
  , "Com RC Outs   : " L.++ showX (outs rc)
  , "Com RC State  : " L.++ showX (sts  rc)
  , "Com Ctxt Ins    : " L.++ showX (ins  c)
  , "Com Ctxt Outs   : " L.++ showX (outs c)
  , "Com Ctxt State  : " L.++ showX (sts  c)
  ]
  where
    prefix = showX (coreId $ ins mut) L.++ "@ "
    ins  (a,_,_) = a
    sts  (_,a,_) = a
    outs (_,_,a) = a

showMeshTrace :: (KnownNat n, KnownNat m) => MeshTrace (n+1) (m+1) -> String
showMeshTrace (MeshTrace ds) = unlines $ "" :
  [ unlines [unwords ["Core", show (x,y), "****"], showPETrace s]
  | x <- [0..length ds-1]
  , y <- [0..length (at d0 ds)-1]
  , let s = ds !! x !! y
  ]

textLog :: forall n m . (KnownNat n, KnownNat m) => [MeshTrace (n+1) (m+1)] -> IO ()
textLog samples = do
  B.writeFile f ""
  -- samples <- vcdToStream "/tmp/heron.vcd" :: IO [MeshTrace (n+1) (m+1)]
  zipWithM_ dump [1..] samples
  where
    f = "/tmp/heron.log"
    dump :: Integer -> MeshTrace (n+1) (m+1) -> IO () -- B.ByteString
    dump ncycle s = B.appendFile f $ B.unwords ["Cycle", B.pack $ show ncycle, "\n", B.pack $ showMeshTrace s]

-- This is a much worse version of just directly tracing mutator phases via vcdTrace
exportPhaseVCD :: forall n m . (KnownNat n, KnownNat m) => SNat (n+1) -> SNat (m+1) -> IO ()
exportPhaseVCD _ _ = do
  samples <- vcdToStream "/tmp/heron.vcd" :: IO [MeshTrace (n+1) (m+1)]
  B.writeFile "/tmp/heron_phase.vcd" $ B.concat [vcdHeader, vcdVars, vcdInit (P.head samples), B.unlines $ P.zipWith vcdStep [0..] samples]
  where
    rows = snatToNum (SNat @(n+1)) :: Int
    cols = snatToNum (SNat @(m+1)) :: Int
    varMap = P.zip [(x,y) | x<-[0..cols-1], y <- [0..rows-1]] [ord '!'..]
    getVar x y = B.pack [chr . fromJust $ lookup (x,y) varMap]
    phaseW = snatToNum (SNat @(BitSize Phase)) :: Int
    signame x y = B.concat ["Core_", B.pack $ show (x,y),"_phase"]
    getP = _phase . (\(_,x,_)->x) . mutator
    getVal = B.pack . P.tail . show . pack
    vcdHeader = B.unlines
      ["$comment No comment $end"
      ,"$timescale 1ps $end"
      ]
    vcdVars = B.unlines $
      "$scope module logic $end" :
      [ B.unwords ["$var wire", B.pack $ show phaseW, getVar x y, signame x y, "$end"]
      | x<-[0..cols-1], y <- [0..rows-1]
      ] P.++
      [ "$upscope $end"
      , "$enddefinitions $end"
      ]
    vcdInit :: MeshTrace (n+1) (m+1) -> B.ByteString
    vcdInit (MeshTrace ds) = B.unlines $
      "#0" :
      "$dumpvars" :
      [ B.unwords [getVal (getP s), v]
      | x<-[0..cols-1], y <- [0..rows-1]
      , let s = ds !! x !! y
      , let v = getVar x y
      ] P.++
      [ "$end" ]
    vcdStep :: Int -> MeshTrace (n+1) (m+1) -> B.ByteString
    vcdStep n (MeshTrace ds) = B.unlines $
      ('#' `B.cons` B.pack (show n)) :
      [ B.unwords [getVal (getP s), v]
      | x<-[0..cols-1], y <- [0..rows-1]
      , let s = ds !! x !! y
      , let v = getVar x y
      ]
