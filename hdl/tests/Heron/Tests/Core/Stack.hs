module Heron.Tests.Core.Stack where

import Prelude
import qualified Clash.Prelude as C
import Control.Monad
import Data.Maybe (listToMaybe)

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.Hedgehog

import Hedgehog ((===))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import GHC.TypeNats
import GHC.Natural (Natural)

import Heron.Core.Stack

genTup :: H.Gen a -> H.Gen b -> H.Gen (a,b)
genTup genA genB
  = do a <- genA
       b <- genB
       pure $ (a,b)

-- Generate a single valid stack command
genStackInput :: H.Gen a
              -> H.Gen (Maybe a, Pop)
genStackInput genA
  = genTup (Gen.maybe genA) Gen.bool

-- Generate a list of coherent valid stack commands
-- We never pop more elements than we actaully have.
genStackInputs :: Int
               -> H.Gen a
               -> H.Gen [(Maybe a, Pop)]
genStackInputs len genA
  = foldM (\xs _ ->
             do let stkSize = sum $ map offset xs
                x <- Gen.filter (\y -> stkSize + offset y > 0)
                                (genStackInput genA)
                pure $ xs ++ [x])
          []
          [1..len]
  where offset (Nothing, False) = 0 :: Int
        offset (Nothing, True ) = (-1)
        offset (Just _ , False) = 1
        offset (Just _ , True ) = 0

golden :: forall d a .
          ( C.KnownNat d
          )
       => [(Maybe a, Pop)] -> [(RamAddr d, Maybe a)]
golden ins = map (\xs -> (fromIntegral $ length xs, listToMaybe xs)) $
             scanl go [] ins
  where
    go stk (Nothing, False) = stk
    go stk (Nothing, True ) = tail stk
    go stk (Just a , False) = a : stk
    go stk (Just a , True ) = a : tail stk

prop_StackGolden :: H.Property
prop_StackGolden = H.withTests 1000 $
                   H.property $ do
  depth :: Natural
        <- H.forAll $ Gen.integral (Range.linear 1 500)

  -- Parameterise on memory depth, with constraint `1 <= d`
  case someNatVal depth of
    SomeNat (_ :: n d) -> do
      case C.compareSNat C.d1 (C.SNat @d) of
        C.SNatGT -> error "Generated stack depth of less than 1"
        C.SNatLE -> do
          -- Ensure we never overflow stack by limiting number of cycles
          inps <- H.forAll (genStackInputs (C.snatToNum (C.SNat @d)-1) (Gen.alpha))
          let want = golden @d inps
          let inps' = (Nothing, False) : inps ++ repeat (Nothing, False)
          let got = take (1 + length inps) $
                    drop 1 $
                    (C.simulate @C.System
                       (fmap (\x -> (_size x, _top x)) .
                        newStack @d (C.blockRam1 C.ClearOnReset (C.SNat @d) Nothing))
                       inps')
          got === want

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
