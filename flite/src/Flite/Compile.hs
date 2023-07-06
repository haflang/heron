module Flite.Compile where

-- Parameterise app-length, spine-length and num apps per template,
-- but not arity limit (for now).

import Flite.Syntax
import Flite.Flatten
import Flite.Frontend
import Data.List
import Flite.Traversals
import Flite.WriterState
import Flite.Inline
import Flite.Predex
import qualified Flite.TemplateSyntax as R

import Flite.Pretty
import Debug.Trace

-- Splits applications so that they contain no more than one 'Alts' node.

splitCase :: App -> Bind App
splitCase app
  | length is <= 1 = return app
  | otherwise = do i <- freshId ; write (i, app0) ; splitCase (Var i:rest)
  where
    is = findIndices isAlts app
    (app0, rest) = splitAt (is !! 1) app

-- Splits an application so that it has maximum length 'n'.

splitApp :: Int -> App -> Bind App
splitApp n app
  | length app <= n = return app
  | otherwise = do i <- freshId ; write (i, app0) ; splitApp n (Var i:rest)
  where (app0, rest) = splitAt n app

-- Splits a group of applications so that they each have maximum
-- length 'n' and no more than one 'Alts' node.

splitApps :: Int -> [(Id, App)] -> [(Id, App)]
splitApps n apps = cs ++ ds
  where
    (i, as, bs) = runWS (mapM splitCase' apps) 0
    (j, cs, ds) = runWS (mapM splitApp' (as ++ bs)) i
    splitCase' (v, app) = (,) v `fmap` (splitCase app)
    splitApp' (v, app) = (,) v `fmap` (splitApp n app)

splitSpine :: Int -> [(Id, App)] -> (App, [(Id, App)], [Exp])
splitSpine n ((v, app):rest) = (spine, rest, luts)
{-
  | length spine <= n = (spine, rest, luts)
  | otherwise =
      ( Var v:takeBack (n-1) spine
      , (v, dropBack (n-1) spine):rest
      , luts
      )
-}
  where
    spine = filter (not . isAlts) app
    luts = filter isAlts app

-- Translates a program to Heron templates. Takes the max application length and
-- max spine length as arguments.

translate :: (InlineFlag, InlineFlag) -> Bool -> Int -> Int -> Int -> Prog -> R.Prog
translate hi strictAnan n m nregs p = map (trDefn n m nregs p2) p2
  where
    p0 = frontend strictAnan nregs hi p
    p1 = [ (f, map getVar args, flatten $ removePredexSpine rhs)
         | Func f args rhs <- p0
         ]
    p2 = lift "main" p1

trDefn n m nregs p (f, args, xs)
  | length args >= floor (2 ^ (ceiling $ logBase 2 $ fromIntegral m))
  = error $ "Flite.Compile.trDefn: Arity of " ++ show f ++ " exceeds maximum"
  | otherwise
  = (f, length args, luts, pushs', apps')
  where
    (spine, body, ls) = splitSpine m xs
    body' = predexReorder nregs $ splitApps n body
    d = (f, args, spine, body')
    luts = map (indexOf p) $ map getAlts ls
    apps = map (trApp p d . snd) body'
    pushs = map (tr p d) $ filter (not . isAlts) spine
    (pushs', apps') = predex nregs (pushs, apps)

trApp p d app
  | isPrimitiveApp app = R.PRIM (-1) rest
  | null luts = R.APP (isNormal rest) rest
  | otherwise = R.CASE (head luts) rest
  where
    app' = rearrange app
    luts = map (indexOf p) $ map getAlts $ filter isAlts app'
    rest = map (tr p d) $ filter (not . isAlts) app'

rearrange (Prim p:x:y:rest) = x:Prim p:y:rest
rearrange app = app

indexOf p f =
  case [i | ((g, args, rhs), i) <- zip p [0..], f == g] of
    [] -> error "Compile: indexOf"
    i:_ -> i

isNormal (R.CON n c:rest) = length rest <= n
isNormal (R.FUN b n f:rest) = length rest < n
isNormal _ = False

tr p d (Int i) = R.INT i
tr p d (Prim f) = R.PRI (primArity f) f
tr p d (Fun f) =
  case xs of
    [] -> R.PRI (primArity f) f
    (i, args):_ -> R.FUN False (length args) i
  where xs = [(i, args) | ((g, args, rhs), i) <- zip p [0..], f == g]
tr p (f, args, spine, body) (Var v) =
  case v `elemIndex` args of
    Nothing -> R.VAR shared idx
    Just i -> R.ARG shared i
  where
    shared = (length $ filter (== v)
                     $ concatMap (concatMap vars) (spine : map snd body)) > 1
    idx = case [i | ((w, _), i) <- zip body [0..], v == w] of
            [] -> error ("Unbound variable: " ++ v)
            i:_ -> i
tr p d (Ctr c n i) = R.CON n i
tr p d Bottom = R.CON 0 66 -- This should never happen

-- Set boolean 'original' flag on funtions; if true, function was
-- originally defined, and if false, function was introduced in
-- Heron compilation process.

flagFuns :: Int -> R.Prog -> R.Prog
flagFuns i p = map flag p
  where
    flag (f, pop, luts, push, apps) =
      (f, pop, luts, map fl push, map (mapAtoms fl) apps)
    fl (R.FUN _ n f) = R.FUN (f < i) n f
    fl a = a

-- Fragment a program such that: (1) each template contains at most
-- 'n' applications; (2) each template contains at most 'm' LUTs; (3)
-- each template pushes a maximum of 'm' atoms; (4) if a template
-- pushes more than one atom, then it contains at most 'n-1'
-- applications; (5) the first atom pushed by the final template does
-- not refer to any of that template's applications (the 'refers
-- check').

fragment :: Int -> Int -> Int -> R.Prog -> R.Prog
fragment s n m p = flagFuns (length p) (p' ++ ts')
  where
    (_, ts, p') = runWS (mapM (frag s n m) p) (length p)
    ts' = map snd (sortBy cmp ts)
    cmp (a, b) (c, d) = compare a c

sub n m = m-n

frag s n m (f, pop, luts, push, apps)
  -- Violated template dimensions
  | length apps > n || length luts > m || length push > s
    = fr s n m (f, pop, luts, push, apps)
  -- We have PRS candidates, so might need to reorder
  | any isPRIM apps = fr s n m (f, pop, luts, push, apps)
  -- OK
  | otherwise = return (f, pop, luts, push, apps)

fr s n m (f, pop, luts, push, apps) =
  do x <- newId
     let offset = length (take n appsHere)
     let (apps0,apps') = (take n appsHere
                         ,map (relocate (sub offset)) (drop n appsHere ++ appsLater))

     let (push0,push') = if length pushHere <= s && null pushLater -- PREVENT EMPTY SPINES ON LAST TEMPLATE
                           then ( []
                                , map (reloc (sub offset)) pushHere)
                           else ( takeBack (s-1) pushHere
                                , map (reloc (sub offset)) (pushLater ++ dropBack (s-1) pushHere))
     let (luts0,luts') = (takeBack m luts, dropBack m luts)
     t <- frag s n m (f, 0, luts', push', apps')
     write (x, t)
     return (f, pop, luts0, (R.FUN False 0 x : push0), apps0)
  where
    (appsHere, appsLater) = splitPredexes apps
    (pushHere, pushLater) = splitSpineByPredexes push apps appsHere
{-
fr s n m (f, pop, luts, push, apps) =
  do x <- newId
     let offset = length (take n apps0)
     let apps' = map (relocate (sub offset)) (drop n apps0 ++ apps1)
     let push' = map (reloc (sub offset)) push
     t <- frag s n m (f, pop, dropBack m luts, push', apps')
     write (x, t)
     return (f, 0, takeBack m luts, [R.FUN False 0 x], take n apps0)
  where
    (apps0, apps1) = splitPredexes apps
-}

relocate f app = mapAtoms (reloc f) app

reloc f (R.VAR sh i) = R.VAR sh (f i)
reloc f x = x

-- Top-level compilation

redCompile :: (InlineFlag, InlineFlag) -> Bool -> Int -> Int -> Int
           -> Int -> Int -> Prog -> R.Prog
redCompile hi strictAnan slen alen napps nluts nregs =
  fragment slen napps nluts . translate hi strictAnan alen slen nregs

-- Auxiliary functions

takeBack n xs = reverse $ take n $ reverse xs

dropBack n xs = reverse $ drop n $ reverse xs

getVar :: Exp -> String
getVar (Var v) = v

vars :: Exp -> [Id]
vars (Var v) = [v]
vars e = []

isAlts :: Exp -> Bool
isAlts (Alts fs n) = True
isAlts e = False

getAlts :: Exp -> Id
getAlts (Alts fs n)
  | null fs = error "RedCompile: getAlts"
  | otherwise = head fs

lift f p = xs ++ ys
  where (xs, ys) = partition (\(g, _, _) -> f == g) p

type Bind a = WriterState (Id, [Exp]) Int a

freshId :: Bind Id
freshId = do n <- get ; set (n+1) ; return ("new_bind_" ++ show n)

type Define a = WriterState (Int, R.Template) Int a

newId :: Define Int
newId = do n <- get ; set (n+1) ; return n
