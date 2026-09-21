module Flite.Strictify
  ( strictifyPrim
  ) where

import           Data.List
import           Flite.CallGraph
import           Flite.Descend
import           Flite.LambdaLift
import           Flite.Syntax
import           Flite.Traversals

isInt (Int i) = True
isInt _       = False

sparkable (Var v) = True
sparkable _       = False

mkApp f []          = f
mkApp (App f es) fs = App f (es ++ fs)
mkApp f es          = App f es

primSatErrMsg :: String
primSatErrMsg = "Applications of primitives must be saturated"

-- Arrange primitive applications according to Memo 40
strictifyPrim :: Prog -> Prog
strictifyPrim = onExp prim
  where
    prim (App (Fun "unwrap") (a:b:rest))
      = result
      where (a', b', rest') = (prim a, prim b, map prim rest)
            result = if isInt a'
                        then mkApp b' (a':rest') -- If the unwrap arg is already evaluated, no need to force it.
                        else mkApp (Fun "unwrap") (a':b':rest')

    prim (App f (a:b:rest))
      | hasId isParSeq f = result
      where (a', b', rest') = (prim a, prim b, map prim rest)
            result = mkApp f (a':b':rest')
              {-
              if sparkable a'
                then mkApp f (a':b':rest')
                else error $ unwords
                       [ "Error: Trying to PAR/SEQ on something other than a variable reference:"
                       , show (App f (a:b:rest))
                       ]
                       -}
    prim (App (Fun f) (a:b:rest))
      | isPredexId f
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
    contents e          = [e]
