module Flite.Syntax where

import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)

type Prog = [Decl]

data Decl = Func { funcName :: Id
                 , funcArgs :: [Pat]
                 , funcRhs  :: Exp }
          | Other String

type Id = String

data AltsTab = AFuns   [Id]          -- Compile to one template base address
             | AInline [([Id], Exp)] -- Compile with inlined single atoms as
  deriving Eq                        -- each alt

data Exp = App Exp [Exp]
         | Case Exp [Alt]
         | Let [Binding] Exp
         | Var Id
         | Con Id
         | Fun Id
         | Int Int
         | Wld -- Wildcard '_'

           -- The following may be introduced by various transformations,
           -- but not by the parser.
         | Bottom
         | Alts AltsTab Int
         | Ctr Id Int Int
         | Lam [Id] Exp

           -- For speculative evaluation of primitive redexes.
         | PrimApp Id [Exp]
         | Prim Id
  deriving Eq

type Pat = Exp

type Alt = (Pat, Exp)

type Binding = (Id, Exp)

type App = [Exp]

-- Primitive functions

unswapPrim :: Id -> Id
unswapPrim p = fromMaybe p (stripPrefix "swap:" p)

isPrimId :: Id -> Bool
isPrimId p = isBinaryPrim p || isUnaryPrim p || isTernaryPrim p

isBinaryPrim :: Id -> Bool
isBinaryPrim = go . unswapPrim
  where
    go "(+)"  = True
    go "(-)"  = True
    go "(==)" = True
    go "(/=)" = True
    go "(<=)" = True
    go "(.&.)"  = True
    go "ld32"  = True
    go _      = False


isUnaryPrim :: Id -> Bool
isUnaryPrim = go . unswapPrim
  where
    go "(!)"  = True
    go "emit" = True
    go "emitInt" = True
    go _ = False

isTernaryPrim :: Id -> Bool
isTernaryPrim = go . unswapPrim
  where
    go "st32" = True
    go _ = False

primArity :: Id -> Int
primArity p
  | isUnaryPrim p = 1
  | isBinaryPrim p = 2
  | isTernaryPrim p = 3

isPredexId :: Id -> Bool
isPredexId = isBinaryPrim
