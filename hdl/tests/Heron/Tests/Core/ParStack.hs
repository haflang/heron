module Heron.Tests.Core.ParStack where

import Prelude hiding (read)
import qualified Clash.Prelude as C
import qualified Clash.Sized.Vector as CV
import Control.Monad
import Data.Maybe (catMaybes, isJust)

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.Hedgehog

import Hedgehog ((===))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import GHC.TypeNats
import GHC.Natural (Natural)

import Heron.Core.ParStack
import Heron.Core.Types

-- Generate a single valid stack command
genStackInput :: forall p a .
                 ( C.KnownNat p
                 )
              => H.Gen a
              -> H.Gen (Maybe (Offset p
                              ,C.Vec (2 C.^ p) (Maybe a)))
genStackInput genA
  = Gen.maybe $
    do let elemsLen = Range.linear 0 p'
       elems <- Gen.list elemsLen genA
       let elems' = CV.unsafeFromList $ take p' $
                    map Just elems ++ repeat Nothing
       offset <- Gen.integral
         (Range.linear minBound
                       (fromIntegral $ min (length elems) (p'-1)))
       pure (offset, elems')
  where
    p' = C.snatToNum (C.SNat :: C.SNat (2 C.^ p))

-- Generate a list of coherent valid stack commands
-- We never pop more elements than we actaully have.
genStackInputs :: forall p a .
                  ( C.KnownNat p
                  )
               => Range.Range Int
               -> H.Gen a
               -> H.Gen [Maybe (Offset p
                               ,C.Vec (2 C.^ p) (Maybe a))]
genStackInputs range genA = do
  is <- Gen.list range (pure ())
  foldM (\xs _ ->
           do let stkSize = sum $ map (maybe (0 :: Int) (fromIntegral . fst)) xs
              let pushSize = maybe 0 (length . catMaybes . C.toList . snd)
              let offset   = maybe 0 (fromIntegral . fst)
              x <- Gen.filter (\y -> min stkSize p' >= pushSize y - offset y)
                              (genStackInput genA)
              pure $ xs ++ [x])
        []
        is
  where p' = C.snatToNum (C.SNat @(2 C.^ p))

golden :: forall p a .
          ( C.KnownNat p
          )
       => [Maybe (Offset p, C.Vec (2 C.^ p) (Maybe a))] -> [C.Vec (2 C.^ p) (Maybe a)]
golden ins = map top' $ scanl go [] ins
  where
    go stk Nothing = stk
    go stk (Just (offset, pushes))
      = let news = filter isJust $ C.toList pushes
        in news ++ drop (length news - fromIntegral offset) stk
    p' = C.snatToNum (C.SNat @(2 C.^ p))
    top' stk = let xs = take p' stk
                   xs' = xs ++ replicate (p' - length xs) Nothing
               in CV.unsafeFromList xs'

prop_ParStackGolden :: H.Property
prop_ParStackGolden = H.withShrinks 0  $
                      H.withTests 1000 $
                      H.property $ do
  depth :: Natural
        <- H.forAll $ Gen.integral (Range.linear (1 :: Natural) 1024)
  let floatDepth :: Float = fromIntegral depth
  par   :: Natural
        <- H.forAll $ Gen.integral (Range.linear 0 (floor $ logBase 2 floatDepth))

  -- Parameterise on stack depth and level of parallelism
  -- There's a lot of constraints to witness here...
  case someNatVal depth of
    SomeNat (_ :: n d) ->
      case someNatVal par of
        SomeNat (_ :: n p) ->
          case C.compareSNat C.d1 (C.SNat @d) of
            C.SNatGT -> error "Invalid ParStack config: d < 1"
            C.SNatLE ->
              case C.compareSNat C.d1 (C.SNat @(d `C.Div` (2 C.^ p))) of
                C.SNatGT -> error $ "Invalid ParStack config: d / (2^p) < 1 " ++ show depth ++ " " ++ show par
                C.SNatLE ->
                  case C.compareSNat C.d1 (C.SNat @(2 C.^ p)) of
                    C.SNatGT -> error "Invalid ParStack config: 2^p < 1"
                    C.SNatLE ->
                      case C.compareSNat (C.SNat @p) (C.SNat @(C.CLog 2 d)) of
                        C.SNatGT -> error "Invalid ParStack config: log_2 d < p"
                        C.SNatLE -> go (C.SNat @p) (C.SNat @d)
  where
    go :: forall p d
        . ( C.KnownNat p
          , C.KnownNat d
          , 1 <= d
          , 1 <= d `C.Div` 2^p
          , 1 <= 2 C.^ p
          , p <= C.CLog 2 d )
       => C.SNat p -> C.SNat d -> H.PropertyT IO ()
    go sp sd = do
           -- Avoid overflows by limiting cycles
           let numCycles = Range.linear 1 ((C.snatToNum sd) `div` (2::Int) ^ (C.snatToInteger sp))
           inps <- H.forAll (genStackInputs numCycles Gen.alpha)

           -- Simulate
           let want = golden @p inps
           let got = take (1 + length inps) $
                     drop 1 $
                     (C.simulate @C.System
                        (fmap read . newCachedParStack @p @d)
                        (Nothing : inps ++ repeat Nothing))
           got === want

-- FIXME Should we allow offsets of +p? Just now it's -p -> p-1

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
