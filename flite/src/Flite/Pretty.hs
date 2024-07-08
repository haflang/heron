module Flite.Pretty where

import           Data.List      (intercalate, nub)
import qualified Data.Map       as Map
import qualified Data.Set       as Set
import           Flite.Case
import           Flite.Fresh
import           Flite.Identify
import           Flite.Matching
import           Flite.Syntax

pretty :: Prog -> String
pretty p = unlines [header, showADTs p, concatMap showDecl p]

header :: String
header = unlines []

showDecl :: Decl -> String
showDecl (Func name args rhs) =
  name ++ " "
  ++ unwords (map (showArg "") args)
  ++ "\n = "
  ++ showExp "   " rhs ++ "\n"

showExp :: String -> Exp -> String
showExp ind (App e es) = unwords (showArg ind e : map (showArg ind) es)
showExp ind (Case e as) = "case " ++ showExp ind e ++ " of" ++ showBlock ("  " ++ ind) showAlt as
showExp ind (Let bs e) = "let " ++ showBlock ("    " ++ ind) showBind bs ++ " in " ++ showExp ("   " ++ ind) e
showExp _   (Var v) = v
showExp _   (Fun f) = f
showExp _   (Prim f) = f
showExp _   (Con c)
  | c == "Cons" = "(:)"
  | c == "Nil"  = "[]"
  | c == "Pair"  = "(,)"
  | otherwise = c
showExp _   (Int i) = show i
showExp _   Bottom = "undefined"
showExp _   (Ctr c _ _) = c
showExp ind (Lam vs e) = '\\' : unwords vs ++ " -> " ++ showExp ind e
showExp _   Wld = "_"
showExp _   e = error $ "Flite.Pretty.showExp: Don't how how to print " ++ show e

showArg :: String -> Exp -> String
showArg ind (App e []) = showArg ind e
showArg ind (App e es) = "(" ++ showExp ind (App e es) ++ ")"
showArg ind (Lam vs e) = "(" ++ showExp ind (Lam vs e) ++ ")"
showArg ind e          = showExp ind e

showBlock :: String -> (String -> a -> String) -> [a] -> String
showBlock ind f as = "\n" ++ ind ++
                     intercalate ("\n"++ind) (map (f ind) as)

showAlt :: String -> Alt -> String
showAlt ind (p, e) = showExp ind p ++ " -> " ++ showExp ("  "++ind) e

showBind :: String -> Binding -> String
showBind ind (v, e) = v ++ " = " ++ showExp ind e

showDCon :: (Id, Int) -> String
showDCon (dcon, arity) = dcon ++ " " ++ unwords args
  where
    args = replicate arity "_"

showTyCon :: (Id, Family) -> String
showTyCon (tycon, dcons) = "data " ++ tycon ++ " =\n" ++ unlines dconStrs
  where
    dconStrs = case map showDCon $ Set.toList dcons of
                (d0:ds) -> ("    " ++ d0) : map ("  | " ++ ) ds
                []      -> []

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
    adts = zip (map (\x->"ADT_"++show x) [0 :: Integer ..])
               [ fam | fam <- nub $ Map.elems ctrs, not (isPrelude fam) ]
    p' = snd (runFresh lessSugar "$" 0)
    lessSugar = desugarCase (identifyFuncs p) >>= desugarEqn
