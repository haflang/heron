module Flite.Flatten (flatten) where

import Flite.Syntax
import Flite.WriterState
import Data.List
import Flite.Traversals
import Control.Monad

expToApp :: Exp -> App
expToApp (App e es) = e:es
expToApp e = [e]

apLen = length . expToApp

type Flatten a = WriterState (Id, App) Int a

intToId :: Int -> Id
intToId i = "tmp_" ++ show i

fresh :: Flatten Id
fresh = do { i <- get ; set (i+1) ; return (intToId i) }

flatten :: Exp -> [(Id, App)]
flatten e
  | length vs /= length (nub vs) = error "Flatten: the impossible happened"
  | otherwise = (intToId i, spine) : binds
  where
    (i, binds, spine) = runWS (flattenSpine e) 0

    vs = map fst binds

flattenSpine :: Exp -> Flatten App
flattenSpine (App (Fun p) (a:rest))
  | isUnaryPrim p
  = do a'    <- flattenSpine a
       rest' <- mapM flattenExp rest
       return $ a' ++ [Fun p] ++ rest'
flattenSpine (App (Fun p) (a:b:rest))
  | isBinaryPrim p
  = do a'    <- flattenSpine a
       b'    <- flattenSpine b
       case rest of
         []     -> return $ a' ++ b' ++ [Fun p]
         (e:es) -> do rest' <- flattenSpine (App e es)
                      return $ a' ++ b' ++ [Fun p] ++ rest'
flattenSpine (App e es) = mapM flattenExp (e:es)
flattenSpine (PrimApp p (a:b:rest))
  = do a' <- flattenExp a
       b' <- flattenExp b
       rest' <- mapM flattenExp rest
       return $ a' : b' : Prim p : rest'
flattenSpine (Let bs e) =
  do (bs', e') <- freshLet (bs, e)
     let (vs, es) = unzip bs'
     mapM flattenSpine es >>= mapM write . zip vs
     flattenSpine e'
flattenSpine e = (:[]) `fmap` flattenExp e

flattenExp :: Exp -> Flatten Exp
flattenExp (App (Fun p) (a:rest))
  | isUnaryPrim p =
  do i <- fresh
     a' <- flattenSpine a
     rest' <- mapM flattenExp rest
     write(i, a' ++ [Fun p] ++ rest')
     return (Var i)
-- FIXME I've hardcoded this for APLEN=4 for heap aps... Naughty
flattenExp (App (Fun p) (a:b:rest))
  | isBinaryPrim p && (apLen a + apLen b < 4) =
  do i <- fresh
     a' <- flattenSpine a
     b' <- flattenSpine b
     rest' <- mapM flattenExp rest
     write(i, a' ++ b' ++ [Fun p] ++ rest')
     return (Var i)
flattenExp (App (Fun p) (a:b:rest))
  | isBinaryPrim p =
  do i <- fresh
     a' <- flattenExp a
     b' <- flattenExp b
     rest' <- mapM flattenExp rest
     write(i, [a', b', Fun p] ++ rest')
     return (Var i)
flattenExp (App e es) =
  do i <- fresh
     app <- mapM flattenExp (e:es)
     write (i, app)
     return (Var i)
flattenExp (PrimApp p (a:b:rest)) =
  do i <- fresh
     a' <- flattenExp a
     b' <- flattenExp b
     rest' <- mapM flattenExp rest
     write (i, a':b':Prim p:rest')
     return (Var i)
flattenExp (Let bs e) =
  do (bs', e') <- freshLet (bs, e)
     let (vs, es) = unzip bs'
     mapM flattenSpine es >>= mapM write . zip vs
     flattenExp e'
flattenExp e = return e

freshLet :: ([Binding], Exp) -> Flatten ([Binding], Exp)
freshLet (bs, e) =
  do ws <- mapM (\_ -> fresh) vs
     let s = zip (map Var ws) vs
     let e' = substMany e s
     let es' = map (flip substMany s) es
     return (zip ws es', e')
  where
    (vs, es) = unzip bs
