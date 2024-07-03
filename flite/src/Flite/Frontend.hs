module Flite.Frontend (frontend) where

import Flite.LambdaLift
import Flite.Syntax
import Flite.Traversals
import Flite.Descend
import Flite.ConcatApp
import Flite.Dependency
import Flite.Matching
import Flite.Case
import Flite.Let
import Flite.Identify
import Flite.Strictify
import Flite.Inline
import Flite.Predex
import Flite.Fresh
import Flite.WorkerWrapper
import Control.Monad
import Data.Either
import Flite.Pretty

frontend :: Bool -> Int -> (InlineFlag, InlineFlag) -> Prog -> Prog
frontend strictAnan nregs i p =
  snd (runFresh (frontendM strictAnan nregs i p) "$" 0)

frontendM :: Bool -> Int -> (InlineFlag, InlineFlag) -> Prog -> Fresh Prog
frontendM strictAnan nregs (h, i) p =
  do p0 <- desugarCase (concatPatApps $ identifyFuncs p) >>= desugarEqn
     let sii = strictIntInfo p0
     p1 <- inlineLinearLet (concatApps p0)
             >>= inlineSimpleLet
             >>= return . lambdaLift 'A'
             >>= return . concApps nregs
             >>= (\p -> return (if strictAnan then workerWrapper sii p else p))
             >>= return . concApps nregs
             >>= inlineTop h
             >>= return . concApps nregs
             >>= return . caseElimWithCaseStack
             >>= return . concApps nregs
             >>= inlineTop i
             >>= return . concApps nregs
             >>= inlineSmallAlts
             >>= return . concApps nregs
             >>= return . identifyPredexCandidates nregs
             >>= return . concatApps
             >>= return . strictifyPrim
             >>= return . concatApps
             >>= return . elimDeadFuns
     return p1

elimDeadFuns :: Prog -> Prog
elimDeadFuns p = [ Func f args rhs | (Func f args rhs) <- p, f `elem` alives ]
  where
    alives = case lookup "main" cg of
      Nothing -> error $ "No main function when eliminating dead code"
      Just xs -> "main":xs
    cg = closure (maybeCallGraph p)

inlineSmallAlts :: Prog -> Fresh Prog
inlineSmallAlts p = onExpM inl p
  where
    isSmall (Func f args (App e []))  = isSmall (Func f args e)
    isSmall (Func f args (Int i))     | i < 64
                                      = Right (toVars args, Int i)
    isSmall (Func f args (Ctr i a b)) | b < 4
                                      = Right (toVars args, Ctr i a b)
    isSmall (Func f args (Var v))     = Right (toVars args, Var v)
    isSmall (Func f _ _)              = Left  ([], Fun f)

    toVars = map (\(Var v) -> v)

    toAlt (Left a) = freshBody a
    toAlt (Right a) = freshBody a

    inl (Alts (AFuns fs) n)
      | length fs <= 2 =
      let alts = map (isSmall . lookupFunc p) fs
      in case any isRight alts of
           False -> return $ Alts (AFuns fs) n
           True  -> do alts' <- mapM toAlt alts
                       return $ Alts (AInline alts') n

    inl e = descendM inl e

concApps :: Int -> Prog -> Prog
concApps 0 = concatApps
concApps nregs = concatNonPrims
