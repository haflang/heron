{-| Handling primitive operations in the Heron Core.
-}
module Heron.Core.Alu
  ( AluIn(..)
  , AluOut
  , alu
  , PrimOpPat(..)
  , primOpPat
  ) where

import Clash.Prelude
import Clash.Annotations.BitRepresentation.Deriving

import Heron.Template

-- | Product type for ALU input, when ready to be evaluated
data AluIn = AluIn OpCode IsSwapped PInt PInt
  deriving (Eq, Show, Generic, NFDataX, ShowX, Lift)

-- | ALU output is a single atom: `PrimInt` for arithmetic or boolean
-- `Con`structor for comparisons.
type AluOut = Atom

-- | Combinatorial ALU logic
alu :: AluIn -> AluOut
alu (AluIn op swp x y) =
  let (x', y') = if swp then (y, x) else (x, y)
  in go op x' y'
  where
    go OpAdd a b = PrimInt $ a+b
    go OpSub a b = PrimInt $ a-b
    go OpEq  a b = fromBool $ a==b
    go OpNeq a b = fromBool $ a/=b
    go OpLeq a b = fromBool $ a<=b
    go OpSeq _ _ = error "Core.Core.alu: SEQ should be handled as a special case"

-- | A view for different stack patterns with primitive operations
data PrimOpPat
  = BothInt PInt (FnArity, IsSwapped, OpCode) PInt
  -- ^ Binary operation with both arguments evaluated
  | FstInt  PInt Atom
  -- ^ Binary operation with only first argument evaluated
  | SndInt  PInt (FnArity, IsSwapped, OpCode)
  -- ^ Binary operation with only second argument evaluated
  | Seq     PInt Atom
  -- ^ Special case for `Seq`
  | NotPrim
  -- ^ Not a primitive application
  deriving (Eq, Generic, NFDataX)
deriveAnnotation (simpleDerivator OneHot OverlapL) [t| PrimOpPat |]
deriveBitPack [t| PrimOpPat |]

-- | Covering function to generate `PrimOpPat`
primOpPat :: forall n
           . (KnownNat n, 3 <= n)
          => Vec n (Maybe Atom) -> PrimOpPat
primOpPat as = leToPlus @3 @n $ go (takeI @3 as)
  where
    go (   Just (PrimInt x)
        :> Just (PrimOp _ _ OpSeq)
        :> Just y
        :> Nil ) = Seq x y
    go (   Just (PrimInt x)
        :> Just (PrimInt y)
        :> Just (PrimOp ar swp op)
        :> Nil ) = BothInt x (ar, swp, op) y
    go (   Just (PrimInt x)
        :> Just (PrimOp ar swp op)
        :> _
        :> Nil ) = SndInt x (ar, swp, op)
    go (   Just (PrimInt x)
        :> Just y
        :> _
        :> Nil ) = FstInt x y
    go _         = NotPrim
