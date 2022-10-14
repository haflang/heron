module Flite.PrettyHaskell where

import Flite.Syntax
import Flite.Case
import Flite.Fresh
import Flite.Matching
import Flite.Identify
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map

consperse :: [a] -> [[a]] -> [a]
consperse x xs = concat (intersperse x xs)

pretty :: Prog -> String
pretty p = unlines [header, showADTs p, concatMap showDecl $ insertSpacers p]

insertSpacers :: Prog -> Prog
insertSpacers p = snd $ foldl (\(curF, acc) f -> if sameF curF f then (curF, acc++[f]) else (newF f, acc++[Other "", f])) ("",[]) p
  where sameF n (Func m _ _) = n==m
        sameF n _ = False
        newF (Func m _ _) = m
        newF _ = "?"

header = unlines []

showDecl (Func name args rhs) =
  name ++ " "
  ++ consperse " " (map (showArg "") args)
  ++ "\n = "
  ++ showExp "   " rhs ++ "\n"
showDecl (Other str) = str ++ "\n"

showExp ind (App e es) = consperse " " (showArg ind e : map (showArg ind) es)
showExp ind (Case e as) = "case " ++ showExp ind e ++ " of" ++ showBlock ("  " ++ ind) showAlt as
showExp ind (Let bs e) = "let " ++ showBlock ("    " ++ ind) showBind bs ++ " in " ++ showExp ("   " ++ ind) e
showExp ind (Var v) = v
showExp ind (Fun f) = f
showExp ind (Prim f) = f
showExp ind (Con c)
  | c == "Cons" = "(:)"
  | c == "Nil"  = "[]"
  | c == "Pair"  = "(,)"
  | otherwise = c
showExp ind (Int i) = show i
showExp ind Bottom = "undefined"
showExp ind (Ctr c arity i) = c
showExp ind (Lam vs e) = '\\' : consperse " " vs ++ " -> " ++ showExp ind e
showExp ind Wld = "_"

showArg :: String -> Exp -> String
showArg ind (App e []) = showArg ind e
showArg ind (App e es) = "(" ++ showExp ind (App e es) ++ ")"
showArg ind (Lam vs e) = "(" ++ showExp ind (Lam vs e) ++ ")"
showArg ind e = showExp ind e

showBlock :: String -> (String -> a -> String) -> [a] -> String
showBlock ind f as = "\n" ++ ind ++
                     consperse ("\n"++ind) (map (f ind) as)

showAlt :: String -> Alt -> String
showAlt ind (p, e) = showExp ind p ++ " -> " ++ showExp ("  "++ind) e

showBind :: String -> Binding -> String
showBind ind (v, e) = v ++ " = " ++ showExp ind e

showDCon :: (Id, Int) -> String
showDCon (dcon, arity) = dcon ++ " " ++ consperse " " args
  where
    args = replicate arity "_"

showTyCon :: (Id, Family) -> String
showTyCon (tycon, dcons) = "data " ++ tycon ++ " =\n" ++ unlines dconStrs
  where
    (d0:ds) = map showDCon $ Set.toList dcons
    dconStrs = ("    " ++ d0) : map ("  | " ++ ) ds

isPrelude :: Family -> Bool
isPrelude fam
  | cons == ["Cons","Nil"] = True
  | cons == ["False","True"] = True
  | cons == ["EQ","GT","LT"] = True
  | cons == ["Just","Nothing"] = True
  | cons == ["Pair"] = True
  | otherwise = False
  where
    cons = map fst $ Set.toList fam

showADTs :: Prog -> String
showADTs p = concatMap showTyCon adts
  where
    ctrs = familyTable $ families p'
    adts = zip (map (\x->"ADT_"++show x) [0..])
               [ fam | fam <- nub $ Map.elems ctrs, not (isPrelude fam) ]
    p' = snd (runFresh lessSugar "$" 0)
    lessSugar = desugarCase (identifyFuncs p) >>= desugarEqn
