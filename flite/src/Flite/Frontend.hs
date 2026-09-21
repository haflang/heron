module Flite.Frontend (frontend, elimDeadFuns) where

import           Data.Either
import           Data.Functor        ((<&>))
import           Flite.Case
import           Flite.ConcatApp
import           Flite.Defunctionalise
import           Flite.Dependency
import           Flite.Descend
import           Flite.Fresh
import           Flite.Identify
import           Flite.Inline
import           Flite.LambdaLift
import           Flite.Let
import           Flite.Matching
import           Flite.Predex
import           Flite.Strictify
import           Flite.Syntax
import           Flite.Traversals
import           Flite.WorkerWrapper
import Flite.Strictness
import Debug.Trace (trace)

frontend :: Bool -> Int -> (InlineFlag, InlineFlag) -> Prog -> Prog
frontend strictAnan nregs i p =
  snd (runFresh (frontendM strictAnan nregs i p) "$" 0)

frontendM :: Bool -> Int -> (InlineFlag, InlineFlag) -> Prog -> Fresh Prog
frontendM strictAnan nregs (h, i) p =
      desugarCase (concatPatApps $ identifyFuncs p)
  >>= desugarEqn
  >>= inlineLinearLet . concatApps
  >>= inlineSimpleLet
  <&> defunctionalise . concApps nregs -- TODO Temporarily disabled defunct
  <&> lambdaLift 'A'
  >>= inlineTop h . concApps nregs
  <&> annStricts
  <&> caseLift . concApps nregs
  <&> annStricts
  <&> workerWrap
  <&> caseElim
  >>= inlineTop i . concApps nregs
  >>= inlineSmallAlts . concApps nregs
  <&> identifyPredexCandidates nregs . concApps nregs
  <&> strictifyPrim . concatApps
  <&> elimDeadFuns . concatApps
  where
    workerWrap p1
      | strictAnan = workerWrapper (strictIntInfo p1) p1
      | otherwise  = p1
    annStricts p1 = annotateStrictArgs (strictnessAnalysis p1) p1
{-
frontendM :: Bool -> Int -> (InlineFlag, InlineFlag) -> Prog -> Fresh Prog
frontkendM strictAnan nregs (h, i) p =
  do p0 <- desugarCase (concatPatApps $ identifyFuncs p)
           >>= desugarEqn
     inlineLinearLet (concatApps p0)
       >>= inlineSimpleLet
       >>= inlineTop h . concApps nregs . lambdaLift 'A'
       >>= inlineTop i . concApps nregs . caseElim . annStricts . workerWrap . caseLift . concApps nregs
       >>= inlineSmallAlts . concApps nregs
       <&> identifyPredexCandidates nregs . concApps nregs
       <&> strictifyPrim . concatApps
       <&> elimDeadFuns . concatApps
  where
    workerWrap p1
      | strictAnan = workerWrapper (strictIntInfo p1) p1
      | otherwise  = p1
    annStricts p1 = annotateStrictArgs (strictnessAnalysis p1) p1
-}

elimDeadFuns :: Prog -> Prog
elimDeadFuns p
  | "id" `elem` alives = p'
  | otherwise          = idF : p'
  where
    idF =  Func "id" [Var "x"] (Var "x")
    p' = [ Func f args rhs | (Func f args rhs) <- p, f `elem` alives ]
    alives = case lookup "main" cg of
      Nothing -> error "No main function when eliminating dead code"
      Just xs -> "main":xs
    cg = closure (maybeCallGraph p)

inlineSmallAlts :: Prog -> Fresh Prog
inlineSmallAlts p = onExpM inl p
  where
    isSmall (Func f args (App e []))  = isSmall (Func f args e)
    isSmall (Func _ args (Int i))     | i < 64
                                      = Right (toVars args, Int i)
    isSmall (Func _ args (Ctr i a b)) | b < 4
                                      = Right (toVars args, Ctr i a b)
    isSmall (Func _ args (Var v))     = Right (toVars args, Var v)
    isSmall (Func f _ _)              = Left  ([], Fun f)

    toVars = map (\(Var v) -> v)

    toAlt (Left a)  = freshBody a
    toAlt (Right a) = freshBody a

    inl (Alts (AFuns fs) n)
      | length fs <= 2 =
      let alts = map (isSmall . lookupFunc p) fs
      in (if any isRight alts then (do alts' <- mapM toAlt alts
                                       return $ Alts (AInline alts') n) else return $ Alts (AFuns fs) n)

    inl e = descendM inl e

concApps :: Int -> Prog -> Prog
concApps 0 = concatApps
concApps _ = concatNonPrims
