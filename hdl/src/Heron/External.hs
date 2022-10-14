{-| Interfaces for external tools in Heron toolchain
-}
module Heron.External
  (compileBenchmark
  ,runEmulator
  )where

import Prelude
import System.Process
import Clash.Prelude (SNat(..), snatToNum)

import Flite.Compile (redCompile)
import Flite.Parse (parseProgFile)
import Flite.Inline (InlineFlag(..))
import qualified Flite.TemplateSyntax as TS
import Heron.Template

-- | Call the F-lite compiler on for a given source file. Automatically infers
--   the project's `SpineLen`, `NodeLen`, and `MaxAps`.
compileBenchmark :: FilePath
                 -- Source file
                 -> IO TS.Prog
                 -- Compiled F-lite templates
compileBenchmark fname = do
  src <- parseProgFile fname
  return $ redCompile (InlineSmall 3, InlineSmall 1)
                      True spineLen nodeLen maxAps 1 maxRegs
                      src
  where
    spineLen = snatToNum $ SNat @MaxPush
    nodeLen  = snatToNum $ SNat @NodeLen
    maxAps   = snatToNum $ SNat @MaxAps
    maxRegs  = snatToNum $ SNat @MaxRegs

-- | Run the external Heron emulator (written in C) on a compiled program.
runEmulator :: TS.Prog
            -- ^ Compiled program
            -> IO (Integer,Integer)
            -- ^ (Return value, Cycle count)
runEmulator tmpl = readProcess "emu" ["-n "++show nodeLen,"-"] (unlines $ map show tmpl)
                   >>= return . read
  where
    nodeLen  = snatToNum $ SNat @NodeLen
