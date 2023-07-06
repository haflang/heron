{-# LANGUAGE GADTs #-}
{-

I've had quite a few errors in automatically derived custom bit representations,
so here we test each data type to ensure that `(unpack . pack) === id`.

Luckily most of the data types are `Enum` and `Bounded` so generating random
members is easy.

-}

module Heron.Tests.Template where

import Prelude hiding (read)
import qualified Clash.Prelude as C
import Clash.Hedgehog.Sized.Vector

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.Hedgehog

import Hedgehog ((===))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import GHC.TypeNats

import Heron.Template
import Heron.Core.Core

ge :: (H.MonadGen m, Enum a, Bounded a) => m a
ge = Gen.enumBounded

genOpCode :: H.Gen OpCode
genOpCode = ge

genPhase :: H.Gen Phase
genPhase = ge

genAtom :: H.Gen Atom
genAtom = Gen.choice
  [ Fun       <$> ge <*> ge <*> ge
  , PrimOp    <$> ge <*> ge <*> genOpCode
  , Ptr       <$> ge <*> ge
  , PrimInt   <$> ge
  , Con       <$> ge <*> ge
  , Arg       <$> ge <*> ge
  , Reg       <$> ge <*> ge
  ]

genAlt :: H.Gen Alt
genAlt = Gen.choice
  [ AFun <$> ge
  , AInt <$> ge <*> ge
  , ACon <$> ge <*> ge <*> ge
  , AArg <$> ge <*> ge
  ]

genCaseTable :: H.Gen (CaseTable Alt)
genCaseTable = Gen.choice
  [ CTInline <$> genAlt <*> genAlt
  , CTOffset <$> ge
  ]

genNode :: (KnownNat a, KnownNat b) => H.Gen (Node a b)
genNode = Gen.choice
  [ Case <$> genCaseTable <*> ge <*> ge <*> genVec (Gen.maybe genAtom)
  , App  <$> ge           <*> ge <*> ge <*> genVec (Gen.maybe genAtom)
  , Prim <$> ge           <*> ge <*> ge <*> genVec (Gen.maybe genAtom)
  ]

genTemplate :: H.Gen Template
genTemplate = Template <$> ge <*> genNode <*> genVec (Gen.maybe genNode)

{- Generating nodes with random lengths...

data SomeNode where
  SomeNode :: C.SNat nApp -> C.SNat nCase -> Node nApp nCase -> SomeNode
deriving instance Show SomeNode

genSomeNode :: H.Gen SomeNode
genSomeNode =
  do (SomeVec n@C.SNat as) <-
       genSomeVec @H.Gen @0 (Range.linear 0 10) (Gen.maybe genAtom)
     Gen.choice $ map (fmap (SomeNode n n))
       [ Case
           <$> genCaseTable <*> ge <*> ge <*> pure as
       , App
           <$> ge <*> ge <*> ge <*> pure as
       , Prim
           <$> ge <*> ge <*> ge <*> genVec (Gen.maybe genAtom)
       ]
-}

checkBitRep :: (Show a, Eq a, C.BitPack a) => H.Gen a -> H.Property
checkBitRep gen = H.withTests 10000 $ H.property $ do
  op <- H.forAll gen
  op === (C.unpack $ C.pack op)

prop_PackOpCode, prop_PackPhase, prop_PackAtom, prop_PackAlt,
  prop_PackCaseTable, prop_PackNode, prop_PackTemplate
  :: H.Property
prop_PackOpCode    = checkBitRep genOpCode
prop_PackPhase     = checkBitRep genPhase
prop_PackAtom      = checkBitRep genAtom
prop_PackAlt       = checkBitRep genAlt
prop_PackCaseTable = checkBitRep genCaseTable
prop_PackNode      = checkBitRep (genNode @4 @3)
prop_PackTemplate  = checkBitRep genTemplate

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
