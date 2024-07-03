module Heron.Tests.Core.Fifo where

import qualified Clash.Prelude       as C
import           Control.Monad
import           Data.Maybe          (listToMaybe)
import           Prelude

import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Test.Tasty.TH

import           GHC.Natural         (Natural)
import           GHC.TypeNats
import           Hedgehog            ((===))
import qualified Hedgehog            as H
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range

import           Heron.Core.Fifo

genTup :: H.Gen a -> H.Gen b -> H.Gen (a,b)
genTup genA genB
  = do a <- genA
       b <- genB
       pure $ (a,b)

-- Generate a single valid fifo command
genFifoInput :: H.Gen a
              -> H.Gen (Maybe a, Pop)
genFifoInput genA
  = genTup (Gen.maybe genA) Gen.bool

-- Generate a list of coherent valid fifo commands.
-- We never pop more elements than we actaully have.
genFifoInputs :: Int
               -> H.Gen a
               -> H.Gen [(Maybe a, Pop)]
genFifoInputs len genA
  = foldM (\xs _ ->
             do let stkSize = sum $ map offset xs
                x <- Gen.filter (\y -> stkSize + offset y > 0)
                                (genFifoInput genA)
                pure $ xs ++ [x])
          []
          [1..len]
  where offset (Nothing, False) = 0 :: Int
        offset (Nothing, True ) = (-1)
        offset (Just _ , False) = 1
        offset (Just _ , True ) = 0

golden :: forall d a .
          ( C.KnownNat d
          , 1 <= d
          )
       => [(Maybe a, Pop)] -> [(RamAddr d, Maybe a)]
golden ins = map (\xs -> (fromIntegral $ length xs, listToMaybe xs)) $
             scanl go [] ins
  where
    go stk (Nothing, False) = stk
    go stk (Nothing, True ) = tail stk
    go stk (Just a , False) = stk ++ [a]
    go stk (Just a , True ) = tail stk ++ [a]

prop_FifoGolden :: H.Property
prop_FifoGolden = H.withTests 1000 $
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
          inps <- H.forAll (genFifoInputs (C.snatToNum (C.SNat @d)-1) (Gen.alpha))
          let want = golden @d inps
          let inps' = (Nothing, False) : inps ++ repeat (Nothing, False)
          let got = take (1 + length inps) $
                    drop 1 $
                    (C.simulate @C.System
                       (fmap (\x -> (_size x, _next x)) .
                        newFifo @d 0 (C.blockRam (C.repeat @d Nothing)))
                       inps')
          got === want

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
