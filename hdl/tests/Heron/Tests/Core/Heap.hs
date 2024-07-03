module Heron.Tests.Core.Heap where

import           Clash.Hedgehog.Sized.Vector
import qualified Clash.Prelude                as C
import           Data.List                    (intersect, mapAccumL, nub)
import           Data.Maybe                   (catMaybes)
import           Prelude                      hiding (read)

import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Test.Tasty.TH

import           GHC.Natural                  (Natural)
import           GHC.TypeNats
import           Hedgehog                     ((===))
import qualified Hedgehog                     as H
import qualified Hedgehog.Gen                 as Gen
import qualified Hedgehog.Range               as Range

import           Heron.Core.Heap
import           Heron.Core.Types
import           Heron.Primitives.DualPortRam

-- Generate a single heap operation
genOp :: forall d a .
         (C.KnownNat d)
      => H.Gen a
      -> H.Gen (C.RamOp d a)
genOp genA
  = Gen.choice
      [ pure C.RamNoOp
      , C.RamRead <$> genAddr
      , do addr <- genAddr
           a    <- genA
           pure $ C.RamWrite addr a
      ]
  where
    d' = maxBound :: RamAddr d
    genAddr = Gen.integral (Range.linear 0 d')

-- Generate sets of simultaneous heap operations without collisions
genOpVec :: forall d p a .
            (C.KnownNat d
            ,C.KnownNat p)
         => H.Gen a
         -> H.Gen (C.Vec p (C.RamOp d a))
genOpVec genA = Gen.filter noCollision $
                genVec (genOp genA)
  where
    noCollision ops
      = let raddrs = catMaybes . map getRAddr $ C.toList ops
            waddrs = catMaybes . map getWAddr $ C.toList ops
            rwCollision = null $ intersect raddrs waddrs
            wwCollision = length waddrs == length (nub waddrs)
        in rwCollision && wwCollision

    getRAddr (C.RamRead a) = Just a
    getRAddr _             = Nothing
    getWAddr (C.RamWrite a _) = Just a
    getWAddr _                = Nothing

genOpVecs :: forall d p a .
             (C.KnownNat d
             ,C.KnownNat p)
          => Range.Range Int
          -> H.Gen a
          -> H.Gen [C.Vec p (C.RamOp d a)]
genOpVecs range genA =
  Gen.list range (genOpVec genA)

updateAt :: Int -> a -> [a] -> [a]
updateAt n x xs = pre ++ [x] ++ post
  where
    (pre, rest) = splitAt n xs
    post = tail rest

-- This implementation follows the double pumped architecture of the UltraRAM
-- blocks. All actions on port A are committed before the actions on port B.
golden :: forall d p a .
          (C.KnownNat d
          ,C.KnownNat p)
       => [C.Vec p (C.RamOp d a)]
       -> [C.Vec p (Maybe a)]
golden ins = snd $ mapAccumL go (replicate d' Nothing) ins
  where
    doOp hp (C.RamNoOp)         = (hp, Nothing)
    doOp hp (C.RamRead addr)    = (hp, hp !! fromIntegral addr)
    doOp hp (C.RamWrite addr x) = (updateAt (fromIntegral addr) (Just x) hp
                                  ,Just x
                                  )
    go hp ops = let (hp', news) = C.mapAccumL doOp hp ops
                in (hp', news)
    d' = C.snatToNum (C.SNat @d)

prop_HeapGolden :: H.Property
prop_HeapGolden = H.withTests 1000 $
                  H.property $ do
  numPorts :: Natural
           <- H.forAll $ Gen.integral (Range.linear 1 2)
  depth    :: Natural
           <- H.forAll $ Gen.integral (Range.linear 1 128)

  -- Parameterise on memory depth, with constraint `1 <= d`
  case someNatVal depth of
    SomeNat (_ :: n d) -> do
      case C.compareSNat C.d1 (C.SNat @d) of
        C.SNatGT -> error "Generated RAM depth of less than 1"
        C.SNatLE -> do

          -- Parameterise on numer of ports on memory, with constraint `p <= 2`
          case someNatVal numPorts of
            SomeNat (_ :: n p) -> do
              case C.compareSNat (C.SNat @p) C.d2 of
                C.SNatGT -> error "Generated input for more than 2 ports"
                C.SNatLE -> do

                  -- Simulate
                  inps <- H.forAll $ genOpVecs @d @p (Range.linear 1 500)
                                                     (Gen.alpha)
                  let inps' = inps ++ repeat (C.repeat C.RamNoOp)
                  let want  = golden inps
                  let got   = map (C.map C.maybeIsX) .
                              take (length inps) $
                              drop 1 $
                              (C.simulate @C.System
                                 (fmap read . newHeap (dpRam UltraRam))
                                 inps')
                  got === want

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
