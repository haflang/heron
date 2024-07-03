module Heron.Tests.Core.Rom where

import           Clash.Hedgehog.Sized.Vector
import qualified Clash.Prelude               as C
import           Prelude                     hiding (read)

import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Test.Tasty.TH

import           Clash.Explicit.ROM.File     (memFile)
import           GHC.Natural                 (Natural)
import           GHC.TypeNats
import           Hedgehog                    ((===))
import qualified Hedgehog                    as H
import qualified Hedgehog.Gen                as Gen
import qualified Hedgehog.Range              as Range
import           System.Directory
import           System.IO.Temp

import           Heron.Core.Rom

golden :: forall d a
        . C.KnownNat d
       => C.Vec d a
       -> [C.Index d]
       -> [a]
golden inits = map (inits C.!!)

prop_RomGolden :: H.Property
prop_RomGolden = H.withTests 1000 $
                 H.property $ do
  depth    :: Natural
           <- H.forAll $ Gen.integral (Range.linear 1 128)
  case someNatVal depth of
    SomeNat (_ :: n d) -> do
      case C.compareSNat C.d1 (C.SNat @d) of
        C.SNatGT -> error "Generated RAM depth of less than 1"
        C.SNatLE -> do
          inits :: C.Vec d (C.Unsigned 8)
                <- H.forAll $ genVec (Gen.integral $ Range.linear 0 maxBound)
          inps  :: [C.Index d]
                <- H.forAll $ Gen.list (Range.linear 1 128) (Gen.integral $ Range.linear 0 maxBound)
          romFile <- H.evalIO $
            writeSystemTempFile "romtest.rom" (memFile (Just 0) inits)

          let want = golden inits inps
          let got  = take (length inps) . drop 1 $
                     C.simulate @C.System (newRom romFile) inps
          got `seq` H.evalIO $ removeFile romFile

          got === want

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
