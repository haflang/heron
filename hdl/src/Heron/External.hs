{-| Interfaces for external tools in Heron toolchain
-}
module Heron.External
  (compileBenchmark
  ,runEmulator
  ,getProjectFile
  )where

import           Clash.Prelude        (SNat (..), snatToNum)
import           Prelude
import           System.Directory     (getCurrentDirectory)
import           System.Process       (readProcess)

import           Data.Functor         ((<&>))
import           Data.List            (isSuffixOf)
import           Flite.Compile        (redCompile)
import           Flite.Inline         (InlineFlag (..))
import           Flite.Parse          (parseProgFile)
import qualified Flite.TemplateSyntax as TS
import           Heron.Parameters

-- | Call the F-lite compiler on for a given source file. Automatically infers
--   the project's `MaxPush`, `NodeLen`, and `MaxAps`.
compileBenchmark :: FilePath
                 -- Source file
                 -> IO TS.Prog
                 -- Compiled F-lite templates
compileBenchmark fname = do
  src <- parseProgFile fname
  return $ redCompile (InlineSmall 3, InlineSmall 1)
                      True spineLen nodeLen maxAps 1 maxRegs maxApSpan
                      src
  where
    spineLen  = snatToNum $ SNat @MaxPush
    nodeLen   = snatToNum $ SNat @NodeLen
    maxAps    = snatToNum $ SNat @MaxAps
    maxRegs   = snatToNum $ SNat @MaxRegs
    maxApSpan = snatToNum $ SNat @MaxApSpan

-- | Run the external Heron emulator (written in C) on a compiled program.
runEmulator :: TS.Prog
            -- ^ Compiled program
            -> IO (Integer,Integer)
            -- ^ (Return value, Cycle count)
runEmulator tmpl = readProcess "emu" ["-n "++show nodeLen,"-"]
                                     (unlines $ map show tmpl) <&> read
  where
    nodeLen  = snatToNum $ SNat @NodeLen :: Int

-- | Helper to resolve project filenames. This allows execution from either
-- Heron's root directory, or this project's subdirectory.
getProjectFile :: FilePath -> IO FilePath
getProjectFile f = (++ f) <$> baseDir
  where
    baseDir = do
      d <- getCurrentDirectory
      if "/hdl" `isSuffixOf` d
        then pure $ d ++ "/"
        else pure $ d ++ "/hdl/"
