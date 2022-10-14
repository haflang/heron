module Heron.Tests.Core.Board where

import Prelude

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.Hedgehog

import Hedgehog ((===))
import qualified Hedgehog as H
import System.Process

import Heron.Core.Board (sim)
import Heron.Template
import Heron.External

assertBenchmark :: FilePath -> H.Property
assertBenchmark prog =
  H.withTests 1 $ H.property $ do
  (ansRet, ansCycles)
    <- H.evalIO (compileBenchmark prog >>= runEmulator)
  (gotCycles, PrimInt gotRet)
    <- H.evalIO (compileBenchmark prog >>= sim (fromIntegral ansCycles+2))
  (fromIntegral gotCycles, fromIntegral gotRet) === (ansCycles, ansRet)

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
