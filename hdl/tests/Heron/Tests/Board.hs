module Heron.Tests.Board where

import           Prelude

import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Test.Tasty.TH

import           Hedgehog            ((===))
import qualified Hedgehog            as H

import           Heron.Board         (sim)
import           Heron.Core.Core     (CPUStats (..))
import           Heron.External
import           Heron.Template

assertBenchmark :: FilePath -> H.Property
assertBenchmark prog =
  H.withTests 1 $ H.property $ do

  prog' <- H.evalIO $ getProjectFile prog
  -- Run emulator for return value and cycle count (sans GC)
  (ansRet, ansCycles)
    <- H.evalIO $
       compileBenchmark prog' >>=
       runEmulator

  -- Pad expected runtime by expected max GC overhead
  let expCycles = ansCycles + maxGcOverhead ansCycles

  -- Run simulation of hardware
  (_, stats, PrimInt gotRet)
    <- H.evalIO $
       compileBenchmark prog >>=
       sim (fromIntegral expCycles)

  -- Eq check
  (fromIntegral (_mutCycles stats), fromIntegral gotRet) === (ansCycles, ansRet)

  where
    maxGcOverhead n = n

prop_BenchAdjoxo, prop_BenchBraun, prop_BenchCichelli, prop_BenchClausify, prop_BenchCountdown, prop_BenchFib, prop_BenchKnuthbendix, prop_BenchMate, prop_BenchMss, prop_BenchOrdlist, prop_BenchPermsort, prop_BenchQueens, prop_BenchQueens2, prop_BenchSumpuz, prop_BenchTaut, prop_BenchWhile
  :: H.Property

prop_BenchAdjoxo      = assertBenchmark "./tests/benchmarks/adjoxo.fl"
prop_BenchBraun       = assertBenchmark "./tests/benchmarks/braun.fl"
prop_BenchCichelli    = assertBenchmark "./tests/benchmarks/cichelli.fl"
prop_BenchClausify    = assertBenchmark "./tests/benchmarks/clausify.fl"
prop_BenchCountdown   = assertBenchmark "./tests/benchmarks/countdown.fl"
prop_BenchFib         = assertBenchmark "./tests/benchmarks/fib.fl"
prop_BenchKnuthbendix = assertBenchmark "./tests/benchmarks/knuthbendix.fl"
prop_BenchMate        = assertBenchmark "./tests/benchmarks/mate.fl"
prop_BenchMss         = assertBenchmark "./tests/benchmarks/mss.fl"
prop_BenchOrdlist     = assertBenchmark "./tests/benchmarks/ordlist.fl"
prop_BenchPermsort    = assertBenchmark "./tests/benchmarks/permsort.fl"
prop_BenchQueens2     = assertBenchmark "./tests/benchmarks/queens2.fl"
prop_BenchQueens      = assertBenchmark "./tests/benchmarks/queens.fl"
prop_BenchSumpuz      = assertBenchmark "./tests/benchmarks/sumpuz.fl"
prop_BenchTaut        = assertBenchmark "./tests/benchmarks/taut.fl"
prop_BenchWhile       = assertBenchmark "./tests/benchmarks/while.fl"

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
