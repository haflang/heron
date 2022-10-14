module Main where

import Heron.Core.Board (topEntity, testBench, sim)
import Heron.Template
import Heron.Encode
import Heron.External
import Flite.Compile (redCompile)
import Flite.Parse (parseProgFile)
import Flite.Inline (InlineFlag(..))

import Prelude
import Control.Monad
import System.Environment
import System.Exit
import Clash.Prelude (pack,unpack,BitSize,SNat(..),snatToNum)
import Clash.Sized.Internal.BitVector
import Clash.Main (defaultMain)

main :: IO ()
main
  = do args <- getArgs

       let fname = last args
       let usage = unlines
                   ["Usage: heron {--clash <clash_options> | -s <flite_src> | -d <flite_src>}"
                   ,""
                   ,"  -s : Run a simulation with the given program as input"
                   ,"  -d : Dump a binary representation of the given program"
                   ]

       when ("-d" `elem` args)
            (compileBenchmark fname
             >>= return . snd . encProg
             >>= dumpTemplates
             >>= print
             >>
             exitWith ExitSuccess)

       when ("-s" `elem` args)
            (compileBenchmark fname
             >>= sim maxBound >>
             exitWith ExitSuccess)

       when ("--clash" `elem` args)
            (defaultMain (tail $ dropWhile (/= "--clash") args) >>
             exitWith ExitSuccess)

       when ("-h" `elem` args)
            (putStrLn usage  >>
             exitWith ExitSuccess)

       putStrLn usage
       exitWith (ExitFailure 1)
  where
    spineLen = snatToNum $ SNat @MaxPush
    nodeLen  = snatToNum $ SNat @NodeLen
    maxAps   = snatToNum $ SNat @MaxAps
    maxRegs  = snatToNum $ SNat @MaxRegs
