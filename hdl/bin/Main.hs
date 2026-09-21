module Main where

import           Heron.Encode
import           Heron.External
import           Heron.Sim          (exportPhaseVCD, simMesh, textLog,
                                     traceMesh)

import           Clash.Main         (defaultMain)
import           Clash.Prelude      (SNat (..), d1, d2, d3, d4)
import           Control.Monad
import           Heron              (dumpTestsuites)
import           Prelude
import           System.Environment
import           System.Exit

meshX :: SNat 2
meshX = SNat

meshY :: SNat 4
meshY = SNat

meshSLRs :: SNat 1
meshSLRs = SNat

main :: IO ()
main
  = do args <- getArgs

       let fname = last args
       let usage = unlines
                   ["Usage: heron {--clash <clash_options> | -s <flite_src> | -d <flite_src> | -p <suffix>}"
                   ,""
                   ,"  -s <max_cycles> : Run a simulation with the given program as input"
                   ,"  -t <max_cycles> : Run a tracing simulation with the given program as input"
                   ,"  -d : Dump a binary representation of the given program"
                   ,"  -p : Dump testsuite as a Python module (for use with PYNQ deployments). Argument is used as a suffix for output filenames."
                   -- ,"  -a : Report the speedups for a given benchmark for a 3-core system and a 7-core system."
                   ]

       when ("-d" `elem` args) $
                 compileBenchmark fname
             >>= dumpTemplates . snd . encProg
             >>= mapM_ putStrLn
             >>  exitSuccess

       when ("-p" `elem` args) $
                 dumpTestsuites fname
             >>  exitSuccess

       when ("-s" `elem` args) $
         let limit = read $ last (init args) :: Int
         in  compileBenchmark fname
             >>= simMesh meshX meshY meshSLRs limit
             >>= print
             >>  exitSuccess

       when ("-t" `elem` args) $
         let limit = read $ last (init args) :: Int
         in  compileBenchmark fname
             >>= traceMesh meshX meshY meshSLRs limit
             >>= print
             -- >>  exportPhaseVCD d2 d4
             >>  exitSuccess

       {-
       when ("-dt" `elem` args) $
         textLog meshX meshY
         >>  exitSuccess

       when ("-dp" `elem` args) $
         exportPhaseVCD meshX meshY
         >>  exitSuccess

       when ("-a" `elem` args) $
             speedups fname
             >>  exitSuccess
       -}

       when ("--clash" `elem` args)
            (defaultMain (tail $ dropWhile (/= "--clash") args) >>
             exitSuccess)

       when ("-h" `elem` args)
            (putStrLn usage  >>
             exitSuccess)

       putStrLn usage
       exitWith (ExitFailure 1)
