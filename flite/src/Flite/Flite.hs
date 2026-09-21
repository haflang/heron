module Flite.Flite (main) where

import           Data.List             (elemIndex)
import           Data.Version          (showVersion)
import           Flite.Compile
import           Flite.Frontend
import           Flite.Inline
import           Flite.Parse
import           Flite.Pretty          (pretty)
import           Flite.Syntax          (showProg)
import qualified Flite.TemplateSyntax  as TS
import           Paths_flite           (version)
import           System.Console.GetOpt
import           System.Environment
import           System.IO
import Flite.Identify (identifyFuncs)

data Flag =
    Desugar
  | Translate
  | CompileToTemplates Int Int Int Int Int Int
  | InlineH (Maybe Int)
  | StrictnessAnalysis
  | InlineI (Maybe Int)
  | PrettyPrint
  deriving Eq

isDisjoint :: Flag -> Bool
isDisjoint (InlineH _)        = False
isDisjoint (InlineI _)        = False
isDisjoint StrictnessAnalysis = False
isDisjoint PrettyPrint        = False
isDisjoint _                  = True

options :: [OptDescr Flag]
options =
  [ Option ['t'] [] (NoArg Translate) "Translate to AST"
  , Option ['d'] [] (NoArg Desugar) "Desugar to subset of AST"
  , Option ['r'] [] (OptArg red "MAXPUSH:APSIZE:MAXAPS:MAXLUTS:MAXREGS:MAXAPSPAN")
                    "Compile to Heron templates"
  , Option ['h'] [] (OptArg (InlineH . fmap read) "MAXAPS")
                    "Inline small function bodies early"
  , Option ['i'] [] (OptArg (InlineI . fmap read) "MAXAPS")
                    "Inline small function bodies late"
  , Option ['s'] [] (NoArg StrictnessAnalysis) "Employ strictness analysis"
  , Option ['p'] [] (NoArg PrettyPrint) "Pretty print templates"
  ]
  where
    redDefaults = CompileToTemplates 6 4 2 1 2 16
    red Nothing = redDefaults
    red (Just s) =
      case split ':' s of
        [a, b, c, d, e, f] ->
          CompileToTemplates (read a) (read b) (read c) (read d) (read e) (read f)
        _ -> error (usageInfo header options)

header :: [Char]
header = "Usage: flite [OPTION...] FILE.fl \n"
      ++ "Version " ++ showVersion version

main :: IO ()
main =
  do args <- getArgs
     case getOpt Permute options args of
       (flags, [fileName], []) -> run flags fileName
       (_, _, errs) -> error (concat errs ++ usageInfo header options)

run :: [Flag] -> FilePath -> IO ()
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
     let ppr = PrettyPrint `elem` flags
     case filter isDisjoint flags of
       [Translate] ->
         let p' = elimDeadFuns $ identifyFuncs p in
         if ppr then putStrLn (pretty p') else print p'
       [Desugar] ->
         putStrLn $ showProg $ frontend sa maxBound (inlineFlagH, inlineFlagI) p
       [CompileToTemplates slen alen napps nluts nregs aspan] ->
         let x = redCompile (inlineFlagH, inlineFlagI) sa slen alen napps nluts nregs aspan p
         in if ppr then mapM_ (putStr . uncurry TS.pretty) (zip [0..] x) else mapM_ print x
       _ -> error (usageInfo header options)

split :: Eq a => a -> [a] -> [[a]]
split x xs =
  case elemIndex x xs of
    Nothing -> [xs]
    Just i -> let (first, rest) = splitAt i xs in
                first : split x (dropWhile (== x) rest)
