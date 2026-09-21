module Flite.Syntax
  ( Decl(..)
  , Prog
  , Id
  , Exp(..)
  , AltsTab(..)
  , Pat
  , Alt
  , Binding
  , App
  , isPredexId
  , isBinaryPrim
  , isUnaryPrim
  , isTernaryPrim
  , isPrimId
  , isParSeq
  , hasId
  , primArity
  , showProg
  ) where

import           Data.List  (intercalate, stripPrefix)
import           Data.Maybe (fromMaybe)

type Prog = [Decl]

data Decl = Func { funcName :: Id
                 , funcArgs :: [Pat]
                 , funcRhs  :: Exp }

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
    go "(+)"    = True
    go "(-)"    = True
    go "(==)"   = True
    go "(/=)"   = True
    go "(<=)"   = True
    go "(.&.)"  = True
    go "ld32"   = True
    go "par"    = True
    go "seq"    = True
    go "unwrap" = True
    go _        = False


isUnaryPrim :: Id -> Bool
isUnaryPrim = go . unswapPrim
  where
    go "emit"    = True
    go "emitInt" = True
    go _         = False

isTernaryPrim :: Id -> Bool
isTernaryPrim = go . unswapPrim
  where
    go "st32" = True
    go _      = False

primArity :: Id -> Int
primArity p
  | isUnaryPrim p = 1
  | isBinaryPrim p = 2
  | isTernaryPrim p = 3
  | otherwise = error $
      "Flite.Syntax.primArity: Expected a primitive application "
      ++ show p

isPredexId :: Id -> Bool
isPredexId "seq" = False
isPredexId "par" = False
isPredexId "unwrap" = False
isPredexId x = isBinaryPrim x

isParSeq :: Id -> Bool
isParSeq "seq" = True
isParSeq "par" = True
isParSeq _ = False

hasId :: (Id -> Bool) -> Exp -> Bool
hasId f (Var id) = f id
hasId f (Fun id) = f id
hasId f (Prim id) = f id
hasId _ _ = False

-- Printing functions

showProg :: Prog -> String
showProg p = "{\n" ++ concatMap show p ++ "}"

instance Show Decl where
  show (Func name args rhs) = name ++ " "
                           ++ unwords (map showArg args)
                           ++ " = "
                           ++ show rhs ++ ";\n"

instance Show AltsTab where
  show (AFuns   fs) = "[Funs : " ++ intercalate "," fs ++ "]"
  show (AInline as) = "[Alts : " ++ intercalate ";" (map showAlt' as) ++ "]"
    where
      showAlt' (args, e) = intercalate "," args ++ " -> " ++ show e

instance Show Exp where
  show (App e es)     = unwords (showArg e : map showArg es)
  show (PrimApp p es) = "{" ++ show (App (Prim p) es) ++ "}"
  show (Case e as)    = "case " ++ show e ++ " of " ++ showBlock showAlt as
  show (Let bs e)     = "let " ++ showBlock showBind bs ++ " in " ++ show e
  show (Var v)        = v
  show (Fun f)        = f
  show (Prim f)       = f
  show (Con c)        = c
  show (Int i)        = show i
  show (Alts as _)    = show as
  show Bottom         = "_|_"
  show (Ctr c _ _)    = c
  show (Lam vs e)     = '\\' : unwords vs ++ " -> " ++ show e
  show Wld            = "_*"

showArg :: Exp -> String
showArg (App e []) = showArg e
showArg (App e es) = "(" ++ show (App e es) ++ ")"
showArg (Lam vs e) = "(" ++ show (Lam vs e) ++ ")"
showArg e          = show e

showBlock :: (a -> String) -> [a] -> String
showBlock f as = "{ " ++ intercalate "; " (map f as) ++ " }"

showAlt :: Alt -> String
showAlt (p, e) = show p ++ " -> " ++ show e

showBind :: Binding -> String
showBind (v, e) = v ++ " = " ++ show e
