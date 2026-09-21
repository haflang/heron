module Heron.Tests.Core.Heap where

import           Clash.Hedgehog.Sized.Vector
import qualified Clash.Prelude                as C
import           Data.List                    (intersect, mapAccumL, nub)
import           Data.Maybe                   (catMaybes)
import           Data.Tuple                   (swap)
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
-- Read port remains latched during write operations
golden :: forall d p a .
          (C.KnownNat d
          ,C.KnownNat p
          ,1<=p)
       => [C.Vec p (C.RamOp d a)]
       -> [C.Vec p (Maybe a)]
golden ins = snd $ mapAccumL go (replicate d' Nothing, C.repeat @p Nothing) ins
  where
    doOp (hp, prevs) (C.RamNoOp) =
      ( (hp, C.rotateLeft prevs 1)
      , C.head prevs
      )
    doOp (hp, prevs) (C.RamRead addr) =
      let rd = hp !! fromIntegral addr
      in ( (hp, prevs C.<<+ rd)
         , rd
         )
    doOp (hp, prevs) (C.RamWrite addr x) =
      let hp' = updateAt (fromIntegral addr) (Just x) hp
      in ( (hp',C.rotateLeft prevs 1)
         , C.head prevs
         )

    go :: ([Maybe a], C.Vec p (Maybe a)) -> C.Vec p (C.RamOp d a)
       -> (([Maybe a], C.Vec p (Maybe a)), C.Vec p (Maybe a))
    go hp ops = C.leToPlus @1 @p $ C.mapAccumL doOp hp ops

    d' = C.snatToNum $ C.SNat @d

simHeap
  :: forall d p a .
     (C.KnownNat d, C.KnownNat p, 1<=d, p<=2, C.NFDataX a)
  => [C.Vec p (C.RamOp d a)]
  -> [C.Vec p (Maybe a)]
simHeap inps =
  let inps' = inps ++ repeat (C.repeat C.RamNoOp)
  in map (C.map C.maybeIsX) .
     take (length inps) $
     drop 1 $
     C.simulate @C.System
       (fmap read . newHeap (dpRam UltraRam)) inps'

prop_HeapConflicts = H.property $ do
  let inp :: [C.Vec 2 (C.RamOp 5 Int)]
          =  [ C.RamWrite 0 42 C.:> C.RamWrite 1 33 C.:> C.Nil
             , C.RamRead  1    C.:> C.RamWrite 1 99 C.:> C.Nil -- R/W -> old data
             , C.RamWrite 0 49 C.:> C.RamRead  0    C.:> C.Nil -- W/R -> new data
             , C.RamWrite 2 22 C.:> C.RamWrite 2 11 C.:> C.Nil -- W/W -> port  B data comitted
             , C.RamRead  2    C.:> C.RamNoOp       C.:> C.Nil -- NoOp -> old data
             ]
      ans =  [ Nothing C.:> Nothing C.:> C.Nil
             , Just 33 C.:> Nothing C.:> C.Nil
             , Just 33 C.:> Just 49 C.:> C.Nil
             , Just 33 C.:> Just 49 C.:> C.Nil
             , Just 11 C.:> Just 49 C.:> C.Nil
             ]
  simHeap inp === ans

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
                C.SNatLE -> case C.compareSNat C.d1 (C.SNat @p) of
                  C.SNatGT -> error "Generated input for less than 1 port"
                  C.SNatLE -> do
                    -- Simulate
                    inps <- H.forAll $ genOpVecs @d @p (Range.linear 1 500)
                                                       (Gen.alpha)
                    simHeap inps === golden inps

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
