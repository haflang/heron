module Main where

import           Heron.Board        (sim)
import           Heron.Encode
import           Heron.External

import           Clash.Main         (defaultMain)
import           Control.Monad
import           Prelude
import           System.Environment
import           System.Exit

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

       when ("-d" `elem` args) $
                 compileBenchmark fname
             >>= dumpTemplates . snd . encProg
             >>= mapM_ putStrLn
             >>  exitSuccess

       when ("-s" `elem` args) $
                 compileBenchmark fname
             >>= sim maxBound
             >>  exitSuccess

       when ("--clash" `elem` args)
            (defaultMain (tail $ dropWhile (/= "--clash") args) >>
             exitSuccess)

       when ("-h" `elem` args)
            (putStrLn usage  >>
             exitSuccess)

       putStrLn usage
       exitWith (ExitFailure 1)
