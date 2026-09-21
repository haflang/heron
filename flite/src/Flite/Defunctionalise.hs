-- | Defunctionalise a program to first-order definitions (source adapted from
-- https://github.com/jmct/FliteDeprest/ and Jose Calderon's thesis)

module Flite.Defunctionalise where
 --  ( defunctionalise
 --  ) where

import Flite.Fresh
import Flite.Traversals
import Flite.Descend
import Flite.Syntax
import Flite.Pretty
import Flite.ConcatApp

import Data.List
import Debug.Trace
import Data.Bifunctor

type Request = (Id, Exp)
type Replacement = (Exp, Exp) -- (from, to)

specSep = "Def"

defunctionalise :: Prog -> Prog
defunctionalise p = p''
    where
        (p', rqss) = unzip [first (Func f args) (gather defunc rhs) | Func f args rhs <- p]
        p'' = case rqs of
            [] -> p'
            _  -> defunctionalise $ theEndlessCycleOfDeathAndRebirth rqs $ concatApps p'
        rqs = concat rqss
        defunc = defuncExp p


-- Transform higher-order function applications to first order.
defuncExp :: Prog -> Exp -> (Exp, [Request])
defuncExp p e@( Fun id )
    | functionExists p id && arityOf p id == 0
        && not ((id `elem`) $ calls rhs) =
            -- inline nullary 'functions' as long as they aren't
            --   recursively defined.
            -- trace ("Inlining " ++ id) $
            (rhs, [])
            where
                Func _ _ rhs = lookupFunc p id
defuncExp p e@( App (Fun id1) ( (Fun id2):as ) )
    | functionExists p id2 && arityOf p id2 > 0 =
        -- trace ("Submitted request: " ++ show rqs) $
        (App (Fun id1') args', rqs)
        where
            id1' = id1 ++ specSep ++ id2
            args' = as
            rqs = [ (id1', e) ]
defuncExp p e@( App (Fun id1) ( (Con id2):as ) )
    | arityOfCon p id2 > 0 =
        -- trace ("Submitted request: " ++ show rqs) $
        (App (Fun id1') args', rqs)
        where
            id1' = id1 ++ specSep ++ id2
            args' = as
            rqs = [ (id1', e) ]
defuncExp p e@( App (Fun id1) ( (App (Fun id2) args2):as ) )
    | functionExists p id2 && arityOf p id2 > length args2 =
        -- trace ("Submitted request: " ++ show rqs) $
        (App (Fun id1') args', rqs)
        where
            id1' = id1 ++ specSep ++ id2
            args' = args2 ++ as
            rqs = [ (id1', e) ]
defuncExp p e@( App (Fun id1) ( (App (Con id2) args2):as ) )
    | arityOfCon p id2 > length args2 =
        -- trace ("Submitted request: " ++ show rqs) $
        (App (Fun id1') args', rqs)
        where
            id1' = id1 ++ specSep ++ id2
            args' = args2 ++ as
            rqs = [ (id1', e) ]
defuncExp p e = (e, [])

theEndlessCycleOfDeathAndRebirth :: [Request] -> Prog -> Prog
theEndlessCycleOfDeathAndRebirth rqs p =
    reaper ["main"] $ stork p rqs

-- Remove unwanted function definitions
reaper :: [Id] -> Prog -> Prog
reaper ids p =
    -- trace (show ids) $
    if length ids == length ids'
        then ds
        else reaper ids' p
    where
        ds = map (lookupFunc p) $ filter (not . isPrimId) ids
        ids' = nub $ ids ++ filter (not . isPrimId) (concatMap (calls . funcRhs) ds)

-- Create a new Decl
stork :: Prog -> [Request] -> Prog
stork p [] = p
stork p ( (id, e):rqs)
    | functionExists p id = stork p rqs
    | otherwise = stork p' rqs where
        p' = d:p
        d = case e of
            ( App (Fun id1) ( (Fun id2):as ) ) ->
                Func id args rhs
                where
                    (Func _ args1 rhs1) = lookupFuncOrPrimitive p id1
                    (Func _ args2 rhs2) = lookupFuncOrPrimitive p id2
                    args = tail args1
                    repls = [ (head args1, Fun id2),
                              (App (Fun id1) args1, App (Fun id) args) ]
                    rhs = replaceAll repls rhs1
            ( App (Fun id1) ( (Con id2):as ) ) ->
                Func id args rhs
                where
                    (Func _ args1 rhs1) = lookupFuncOrPrimitive p id1
                    args2 = getUniqueVars p $ arityOfCon p id2
                    args = tail args1
                    repls = [ (head args1, Con id2),
                              (App (Fun id1) args1, App (Fun id) args) ]
                    rhs = replaceAll repls rhs1
            ( App (Fun id1) ( app@(App (Fun id2) as2):as1 ) ) ->
                Func id args rhs
                where
                    (Func _ args1 rhs1) = lookupFuncOrPrimitive p id1
                    (Func _ args2 rhs2) = lookupFuncOrPrimitive p id2
                    args2' = take (length as2) args2
                    args = args2' ++ tail args1
                    repls = [ (head args1, App (Fun id2) args2'),
                              (App (Fun id1) args1, App (Fun id) args) ]
                    rhs = replaceAll repls rhs1
            ( App (Fun id1) ( app@(App (Con id2) as2):as1 ) ) ->
                Func id args rhs
                where
                    (Func _ args1 rhs1) = lookupFuncOrPrimitive p id1
                    args2 = getUniqueVars p $ arityOfCon p id2
                    args2' = take (length as2) args2
                    args = args2' ++ tail args1
                    repls = [ (head args1, App (Con id2) args2'),
                              (App (Fun id1) args1, App (Fun id) args) ]
                    rhs = replaceAll repls rhs1
            _ -> error $ "Don't know how to satisfy request for " ++ show (id, e)

-- Some utility functions...

replaceAll :: [Replacement] -> Exp -> Exp
replaceAll rs = fst . gather (replace rs)

replace :: [Replacement] -> Exp -> ( Exp, [a] )
replace rs exp =
    case subs of
        [] -> (exp, [])
        [(_, to)] -> (to, [])
        _ -> error $ "Found multiple possible replacements for " ++ show exp ++ ". Not sure what to do!"
    where
        subs = filter ( (exp ==) . fst ) rs

functionExists :: Prog -> Id -> Bool
functionExists p id = isPrimId id || elem id [fid | Func fid _ _ <- p]

lookupFuncOrPrimitive :: Prog -> Id -> Decl
lookupFuncOrPrimitive p id
    | isBinaryPrim id = Func id args2 (App (Fun id) args2)
    | isUnaryPrim id  = Func id args1 (App (Fun id) args1)
    | otherwise = case lookupFuncs id p of
        [d] -> d
        _ -> error $ "Couldn't find Decl for " ++ id ++ " in \n\n" ++ show p
    where
        args2 = getUniqueVars p 2
        args1 = getUniqueVars p 2


arityOf :: Prog -> Id -> Int
arityOf p id = length $ funcArgs f
    where
        f = lookupFuncOrPrimitive p id

arityOfCon :: Prog -> Id -> Int
arityOfCon p id = head $ concat $ arities
    where
        arities = fromExp getCon p
        getCon e = map (concatMap getArity) (caseAlts e)
        getArity (App (Con cid) ps, e) | cid == id = [length ps]
        getArity (p, e) = []

-- Generate a list of Args with ids that aren't already found in Prog
getUniqueVars :: Prog -> Int -> [Exp]
getUniqueVars _ 0 = []
getUniqueVars p n = take n $ newIds
    where
        newIds = filter (not . (`elem` exIds)) [ Var ('?':(show i)) | i <- [1..] ]
        exIds = fromExp getVarIds p
        getVarIds (Var id) = [Var id]
        getVarIds _ = []

-- TODO This should probably just be in the Fresh monad instead of using getUniqueVars
