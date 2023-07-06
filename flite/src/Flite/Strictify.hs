module Flite.Strictify
  ( strictifyPrim
  ) where

import Flite.Syntax
import Flite.Traversals
import Flite.Descend
import Flite.CallGraph
import Data.List
import Flite.LambdaLift

isInt (Int i) = True
isInt _ = False

mkApp f [] = f
mkApp (App f es) fs = App f (es ++ fs)
mkApp f es = App f es

primSatErrMsg :: String
primSatErrMsg = "Applications of primitives must be saturated"

-- Arrange primitive applications according to Memo 40
strictifyPrim :: Prog -> Prog
strictifyPrim = onExp prim
  where
    prim (App (Fun f) (a:b:rest))
      | isUnaryPrim f = result
      where (a', b', rest') = (prim a, prim b, map prim rest)
            result = if isInt a'
                        then mkApp (b') (a':rest') -- If the SEQ arg is already evaluated, no need to force it.
                        else mkApp (Fun f) (a':b':rest')

    prim (App (Fun f) (a:b:rest))
      | isBinaryPrim f
      = result
      where (a', b', rest') = (prim a, prim b, map prim rest)
            result = if isInt a' && not (isInt b')
                       then mkApp (Fun ("swap:"++f)) (b':a':rest')
                       else mkApp (Fun f) (a':b':rest')
    prim (App (Fun f) es)
      | isPrimId f = error primSatErrMsg
    prim (Fun f)
      | isPrimId f = error primSatErrMsg
    prim e = descend prim e

catApp :: [Exp] -> Exp
catApp es = App x xs
  where
    x:xs = concatMap contents es
    contents (App e es) = e:es
    contents e = [e]
