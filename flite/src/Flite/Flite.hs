module Flite.Flite (main) where

import Paths_flite (version)
import Data.Version (showVersion)
import Flite.Syntax
import Flite.Parse
import Flite.Pretty
import Flite.Inline
import Flite.Compile
import Flite.Frontend
import qualified Flite.PrettyHaskell as PH
import Data.List
import System.IO
import System.Environment
import System.Console.GetOpt

data Flag =
    Desugar
  | Translate
  | CompileToTemplates Int Int Int Int Int
  | InlineH (Maybe Int)
  | StrictnessAnalysis
  | InlineI (Maybe Int)
  | VerboseResult
  deriving Eq

isDisjoint (InlineH i) = False
isDisjoint (InlineI i) = False
isDisjoint StrictnessAnalysis = False
isDisjoint VerboseResult = False
isDisjoint flag = True

options :: [OptDescr Flag]
options =
  [ Option ['d'] [] (NoArg Desugar) "desugar"
  , Option ['t'] [] (NoArg Translate) "translate"
  , Option ['r'] [] (OptArg red "MAXPUSH:APSIZE:MAXAPS:MAXLUTS:MAXREGS")
                    "compile to Heron templates"
  , Option ['h'] [] (OptArg (InlineH . fmap read) "MAXAPS")
                    "inline small function bodies early"
  , Option ['i'] [] (OptArg (InlineI . fmap read) "MAXAPS")
                    "inline small function bodies late"
  , Option ['s'] [] (NoArg StrictnessAnalysis) "employ strictness analysis"
  , Option ['v'] [] (NoArg VerboseResult) "show the integer result from main"
  ]
  where
    redDefaults = CompileToTemplates 6 4 2 1 8
    red Nothing = redDefaults
    red (Just s) =
      case split ':' s of
        [a, b, c, d, e] ->
          CompileToTemplates (read a) (read b) (read c) (read d) (read e)
        _ -> error (usageInfo header options)

header = "Usage: Flite [OPTION...] FILE.hs \n"
      ++ "Version " ++ showVersion version

main =
  do args <- getArgs
     case getOpt Permute options args of
       (flags, [fileName], []) -> run flags fileName
       (_, _, errs) -> error (concat errs ++ usageInfo header options)

run flags fileName =
  do hSetBuffering stdout NoBuffering
     p <- parseProgFile fileName
     let inlineFlagH = head $ [InlineAll | InlineH Nothing <- flags]
                          ++ [InlineSmall i | InlineH (Just i) <- flags]
                          ++ [NoInline]
     let inlineFlagI = head $ [InlineAll | InlineI Nothing <- flags]
                          ++ [InlineSmall i | InlineI (Just i) <- flags]
                          ++ [NoInline]
     let sa = StrictnessAnalysis `elem` flags
     case filter isDisjoint flags of
       [Translate] ->
         putStrLn $ PH.pretty p
       [Desugar] ->
         putStrLn $ pretty $ frontend sa maxBound (inlineFlagH, inlineFlagI) p
       [CompileToTemplates slen alen napps nluts nregs] ->
         mapM_ print $ redCompile (inlineFlagH, inlineFlagI) sa slen alen napps nluts nregs p
       _ -> error (usageInfo header options)

-- Auxiliary

split :: Eq a => a -> [a] -> [[a]]
split x xs =
  case elemIndex x xs of
    Nothing -> [xs]
    Just i -> let (first, rest) = splitAt i xs in
                first : split x (dropWhile (== x) rest)
