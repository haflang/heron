{-# LANGUAGE NumericUnderscores, ApplicativeDo, RecordWildCards, FlexibleContexts #-}

{-| Board-level orchestration of the Heron Core. Marries up control logic from
    `Heron.Core.Core` with its memory components.
-}
module Heron.Core.Board
  (
  -- * Simulation
    sim
  -- * Full system
  , board
  -- * Top-level synthesisable circuits
  , topEntity
  , testBench
  ) where

import Clash.Prelude hiding (read)
import qualified Clash.Explicit.Prelude as E
import Clash.Explicit.Testbench
import Clash.Annotations.TH
import Language.Haskell.TH
import Heron.Core.Types
import Heron.Core.Core
import Heron.Core.ParStack hiding (topEntity)
import Heron.Core.Stack
import Heron.Core.Heap
import Heron.Core.Alu
import Heron.Template
import Heron.TemplateTH
import Heron.Encode (encProg)
import Heron.External
import qualified Flite.TemplateSyntax as TS
import Heron.Xilinx.DualPortRam
import Heron.Xilinx.DistributedRam
import Heron.Xilinx.ClockDiv hiding (topEntity, testBench)

import RetroClash.Barbies
import qualified Data.List as L
import Data.Maybe (isJust, catMaybes, fromMaybe)
import Language.Haskell.TH hiding (Type)

-- | Complete system with all control inputs and outputs exposed
board
  :: (HiddenClockResetEnable dom)
  => Heap dom HeapNode 2 HeapSize
  -- ^ Heap memory architecture
  -> Heap dom Template 2 RomSize
  -- ^ Template memory architecture
  -> Signal dom (RamOp RomSize Template)
  -- ^ External interface into template memory (for loading user application)
  -> Signal dom (Maybe TemplAddr       )
  -- ^ Signal to start evaluation at given template address
  -> ( Signals dom CPUIn
     , Signals dom CPUOut
     )
  -- ^ Expose all control signals for debugging
board heapPrim tmplPrim romCtrl begin = (CPUIn{..}, CPUOut{..})
  where
    CPUOut{..} = cpu CPUIn{..}

    uStkRam = blockRam1 ClearOnReset (SNat @UStkSize) Nothing
    aStkRam = blockRam1 ClearOnReset (SNat @AStkSize) Nothing
    pStkRam = blockRam1 ClearOnReset (SNat @PStkSize) Nothing

    uStkIn = newStack uStkRam $ bundle (_uStkPush, _uStkPop)
    aStkIn = newStack aStkRam $ bundle (_aStkPush, _aStkPop)
    pStkIn = newStack pStkRam $ bundle (_pStkPush, _pStkPop)
    vStkIn = newCachedParStack _vStkOut
    heapIn = heapPrim _heapOut
    tmplIn = (head . read) <$>
               (tmplPrim . bundle $
               (RamRead <$> _tmplOut) :> romCtrl :> Nil)

-- | Generate the two related clocks required by our memory multi-pumping techniques
boardClkGen
  :: forall domIn domFast domSlow periodIn edge reset init polarity a b
   . ( DividedClks 2 domFast domSlow periodIn edge reset init polarity
   ,   KnownConfiguration domIn ('DomainConfiguration domIn periodIn edge reset init polarity) )
  => Clock domIn -> Reset domIn -> Enable domIn
  -> ( Clock domFast, Reset domFast, Enable domFast
     , Clock domSlow, Reset domSlow, Enable domSlow )
boardClkGen clkIn rstIn enIn = (clkFast, rstFast, enFast, clkSlow, rstSlow, enSlow)
  where
    (clkFast, clkSlow) = clockDiv @2 @domIn @domFast d2 clkIn rstIn (toEnable $ pure True)
    rstFast = E.convertReset clkIn clkFast rstIn
    rstSlow = E.convertReset clkIn clkSlow rstIn
    enFast = toEnable $ pure True
    enSlow = toEnable $ pure True

-- | A version of `board` with fewer input/output pins. Makes it easier to
-- synthesise in Vivado in isolation.
boardFewerIOBs
  :: (HiddenClockResetEnable dom)
  => Heap dom HeapNode 2 HeapSize
  -> Heap dom Template 2 RomSize
  -> Signal dom (RamOp RomSize Template) -- Interface to template memory
  -> Signal dom Bool                     -- Start reduction
  -> Signal dom (Maybe Atom)
boardFewerIOBs heapPrim tmplPrim tOp go = delay Nothing $ delay Nothing out
  where
    out = fmap (maybe Nothing head) . _result . snd $ board heapPrim tmplPrim tOp goAddr
    goAddr = mux go (pure $ Just 0) (pure Nothing)

createDomain vSystem{vName="DomIn"  , vPeriod=__ClkT__}
createDomain vSystem{vName="DomFast", vPeriod=__ClkT__}
createDomain vSystem{vName="DomSlow", vPeriod=2*__ClkT__}

-- | The main synthesisable system with explicit clock, resets, enables, and
-- port names.
topEntity
  :: "clk" ::: Clock DomIn
  -- ^ Input clock
  -> "rst" ::: Reset DomIn
  -- ^ Reset
  -> "en"  ::: Enable DomIn
  -- ^ Enable
  -> "codeWE"   ::: Signal DomIn Bool
  -- ^ Template memeory write enable
  -> "codeAddr" ::: Signal DomIn TemplAddr
  -- ^ Template memeory address
  -> "codeData" ::: Signal DomIn Template
  -- ^ Template memeory data
  -> "go"       ::: Signal DomIn Bool
  -- ^ Start signal
  -> ("ret"    ::: Signal DomIn Atom
     ,"retVld" ::: Signal DomIn Bool
     )
  -- ^ Return result atom
topEntity clkIn rstIn enIn cWe cAddr cData go =
  ( fmap (fromMaybe (unpack 0)) ret
  , fmap isJust                 ret)
  where
    cOp = mux cWe (RamWrite <$> cAddr <*> cData)
                  (pure RamNoOp)

    wrapHeap = withClockResetEnable @DomIn clkIn rstIn enIn $
               newHeap $
               dpRam @HeapSize @DomIn @HeapNode UltraRam

    wrapTmpl = withClockResetEnable @DomIn clkIn rstIn enIn $
               newHeap $
               dpRam @RomSize @DomIn @Template BlockRam

    ret = withClockResetEnable @DomIn clkIn rstIn enIn $
          (boardFewerIOBs @DomIn wrapHeap wrapTmpl cOp go)

makeTopEntity 'topEntity

-- | Simulate `board` at a Haskell-level
sim :: Int
    -- ^ Bound on cycle count before termination (pass `maxBound` for maximumg)
    -> TS.Prog
    -- ^ Input program as F-lite templates
    -> IO (Int, Atom)
    -- ^ (Simulated cycle count, return value)
sim limit p = do
  -- Test input loads program into code memory, then says GO
  let (initAddr, prog) = encProg p
  let simIn  = L.concat
                 [ L.zipWith (\a t -> (RamWrite a t, Nothing)) [0..] prog
                 , [(RamNoOp, Just initAddr)]
                 , L.repeat (RamNoOp, Nothing)
                 ]
  -- Run sim
  let simOut = L.take limit $ L.drop (1 + L.length prog) $
               simulate @System (reshape (board (newHeap (dpRam UltraRam)) (newHeap (dpRam UltraRam)))) simIn
               -- TODO We currently simulate without the fast clock domain for
               -- code/heap memories but topEntity and testBench _do_. Best to
               -- make this consistent?

  -- Report results
  let doneAt = L.findIndex (isJust . _result . snd) simOut
  case doneAt of
    Nothing  -> do
      putStrLn $ showSample (simOut L.!! (limit-1))
      error $ L.concat ["Core still running after ", show limit, " cycles. Aborting."]
    Just idx -> do putStrLn $ L.concat ["Finished after ", show (idx-1), " cycles: "]
                   putStrLn $ showSample (simOut L.!! idx)
                   let res = top . vStkIn . fst $ simOut L.!! idx
                   pure (idx-1, res)

  where
    reshape dut = bundle . (\(a,b)->(bundle a, bundle b)) . uncurry dut . unbundle
    showMVec v = showX . catMaybes $ toList v
    showSample (is, os) = L.unlines
      [ "Stk top   : " L.++ showMVec (read (vStkIn is))
      , "Heap Ctrls: " L.++ showX  (_heapOut os)
      , "Result    : " L.++ maybe "Running..." showMVec (_result os)
      , "AStk top  : " L.++ showX (read (aStkIn is))
      , "Outs      : " L.++ showX os
      , "Ins       : " L.++ showX is
      ]

-- | Simulate `board` at a verilog-level. Uses Clash's `outputVerifier'` to
-- produce a verilog testbench. Assertion errors will be thrown if answer is
-- wrong, too early, or too late.
--
-- For now, this is specialised to only the adjoxo benchmark using Template
-- Haskell.
testBench :: Signal DomIn Bool
testBench = done
 where
  -- Use Template Haskell to run C emulator and return benchmark's
  -- expected cycles (as type-level Nat), expected return atom,
  -- and it's compiled templates (as a Vec)
  (expCycles, expAtom, tmpls)
            = $( do prog <- runIO (compileBenchmark "tests/benchmarks/adjoxo.fl")
                    let tmpls = snd (encProg prog)
                    tmplsVec <- listToVecTH tmpls
                    (expRet, expCycles) <- runIO (runEmulator prog)
                    expRetAtom <- [| PrimInt (fromIntegral expRet) |]
                    expCyclesS <- promoteIntTH expCycles
                    let tup = TupE [Just expCyclesS, Just expRetAtom, Just tmplsVec]
                    return tup
               )
  defTmpl = unpack $ minBound
  tmplsFull :: Vec RomSize Template = tmpls ++ repeat defTmpl

  testInput = unzip $ replicate d30 (tmplNoOp, False) ++
                      -- Wait for enable/reset
                      zipWith (\t a -> ((True, a, t), False)) tmplsFull (iterateI (+1) 0) ++
                      -- Write templates to memory
                      singleton (tmplNoOp, True) ++
                      -- Start reduction
                      replicate (addSNat d5 expCycles) (tmplNoOp, False)
                      -- Wait until we expect an answer
  tmplNoOp = (False, 0, defTmpl)
  (tmplWe, tmplAddr, tmplData)
     = unbundle $ stimuliGenerator clkIn rstIn (fst testInput)
  go = stimuliGenerator clkIn rstIn (snd testInput)
  expectOutput = outputVerifier' clkIn rstIn (
                   replicate (addSNat (SNat @(34+RomSize)) expCycles) (unpack 0) ++
                   singleton expAtom)
  selRet (ret,_) = ret
  done         = expectOutput (selRet $ topEntity clkIn rstIn enIn tmplWe tmplAddr tmplData go)

  enIn           = enableGen  @DomIn
  clkIn          = tbClockGen @DomIn (fmap not done)
  rstIn          = resetGen   @DomIn
