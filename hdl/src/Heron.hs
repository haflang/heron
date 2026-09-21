-- |
-- Module      : Heron
-- Description : The Heron processor for acceleration of functional languages
-- Copyright   : (c) Craig Ramsay, 2023
--                   HAFLANG Project, 2023
-- License     : BSD-3
-- Maintainer  : craig.ramsay@hw.ac.uk
-- Stability   : experimental
-- Portability : POSIX
--
-- The synthesisable description of the Heron Core processor. We accelerate the
-- evaluation of functional programs written in F-lite by directly matching the
-- evaluation model of template instantiation, rather than using complex, deeply
-- pipelined architectures with compiled graph reduction.
--
-- See our project homepage at [haflang.github.io](http://haflang.github.io/). Our
-- curated [history of specialised graph reduction
-- machines](http://haflang.github.io/history.html) might be a good place to start.
module Heron
  ( sim,
    topEntity,
    -- testBench,
    compileBenchmark,
    dumpTemplates,
    dumpTestsuites,
    runEmulator,
    SNat(..)
  )
where

import           Clash.Prelude  (SNat (..))
import           Control.Monad
import           Heron.Board
import           Heron.Encode
import           Heron.External
import           Heron.Sim
import           Prelude
import           System.IO

benches :: [String]
benches =
  [ "adjoxo",
    "braun",
    "cichelli",
    "clausify",
    "countdown",
    "fib",
    "knuthbendix",
    "mate",
    "mss",
    "ordlist",
    "permsort",
    "queens2",
    "queens",
    "sumpuz",
    "taut",
    "while"
  ]

-- | Dump binaries and return values for the entire testsuite as Python modules.
-- This can be used with our Ultra96 PYNQ prototyping setup.
dumpTestsuites :: FilePath -> IO ()
dumpTestsuites f = zipWithM_ dumpTestsuite dirs fs
  where
    dirs =
      [ "../flite/examples/small/",
        "../flite/examples/large/"
      ]
    fs =
      [ "./smalls_" ++ f ++ ".py",
        "./larges_" ++ f ++ ".py"
      ]

dumpTestsuite :: FilePath -> FilePath -> IO ()
dumpTestsuite srcDir f = do
  (ns, rets, progs) <- unzip3 <$> mapM dumpOne benches
  h <- openFile f WriteMode
  hPutStr h "codes = dict("
  hPutStrLn h . consperse "\n            ," $ zipWith (\n p -> n ++ " = " ++ show p) ns progs
  hPutStrLn h ")"
  hPutStr h "rets  = dict("
  hPutStrLn h . consperse "\n            ," $ zipWith (\n p -> n ++ " = " ++ show p) ns rets
  hPutStr h ")"
  hClose h
  where
    fullPath b = srcDir ++ b ++ ".fl"

    dumpOne b = do
      putStrLn b
      p <- getProjectFile (fullPath b) >>= compileBenchmark
      (emuRet, _) <- runEmulator p
      bits <- dumpTemplatesPy . snd $ encProg p
      return (sanitiseName b, emuRet, bits)

    consperse sep = foldl1 (\x y -> x ++ sep ++ y)

    sanitiseName "while" = "while_"
    sanitiseName x       = x

{-
speedups :: FilePath -> IO (Double)
speedups f = do
  p <- compileBenchmark f
  (t1,_,r1) <- sim  maxBound p
  (t2x2,_,r2x2) <- sim2x2 (2*t1) p
  let report t = fromIntegral t1 / fromIntegral t
      sp2x2 = report t2x2
  when (r2x2 /= r1) (error "Inconsistent results!")
  putStrLn $ unwords ["1 ," , show sp2x2]
  return (sp2x2)
-}
