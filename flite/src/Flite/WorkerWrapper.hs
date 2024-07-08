module Flite.WorkerWrapper
  ( StrictIntInfo       -- type StrictIntInfo = [(Id, [Bool])]
  , strictIntInfo       -- :: Prog -> StrictIntInfo
  , workerWrapper       -- :: StrictIntInfo -> Prog -> Prog
  ) where

import           Data.List
import           Flite.Dependency
import           Flite.Descend
import           Flite.IntInfer
import           Flite.Strictness
import           Flite.Syntax
import           Flite.Traversals

-- Stores the strict integer args of each function
type StrictIntInfo = [(Id, [Bool])]

-- Computes the strict integer arguments of each function
strictIntInfo :: Prog -> StrictIntInfo
strictIntInfo p = map ub p
  where
    is = intInfer p
    ss = strictnessAnalysis p
    ub d = (funcName d, zipWith (&&) (retrieve (funcName d) ss)
                                     (retrieve (funcName d) is))

retrieve :: String -> [(String, b)] -> b
retrieve k m =
 case lookup k m of
   Nothing -> error ("WorkerWrapper.retrieve: function does not exist: " ++ k)
   Just b  -> b

-- Worker-wrapper transformation
-- Wrapper forces strict integer args before invoking worker
workerWrapper :: StrictIntInfo -> Prog -> Prog
workerWrapper sii p = map (wrap cg sii) p ++ new
  where
    new = concatMap (wrapper sii) p
    cg = closure (callGraph p)

wrapper :: StrictIntInfo -> Decl -> [Decl]
wrapper sii d
  | or siArgs = wr [e | (True, e) <- zip siArgs (funcArgs d)] (funcArgs d) d 0
  | otherwise = []
  where siArgs = retrieve (funcName d) sii

wr :: [Exp] -> [Exp] -> Decl -> Int -> [Decl]
wr [] args d n =
  [ Func { funcName = funcName d ++ "'" ++ show n
         , funcArgs = args
         , funcRhs  = App (Fun (funcName d)) (funcArgs d) } ]
wr (e:es) args d n =
  Func
    { funcName = funcName d ++ "'" ++ show n
    , funcArgs = args
    , funcRhs  = App (Fun "unwrap") (e:f:restArgs)
    } : wr es (e:restArgs) d (n+1)
  where
    f = Fun (funcName d ++ "'" ++ show (n+1))
    restArgs = [x | x <- args, varId x /= varId e]

varId :: Exp -> Id
varId (Var v) = v
varId e       = error "WorkerWrapper: varId called on non-variable"

-- Replace calls to a worker f with calls to wrapper f' provided that
-- the call to f occurs in a function that is NOT call-reachable from f.
wrap :: DepGraph -> StrictIntInfo -> Decl -> Decl
wrap cg sii d = d { funcRhs = wrapExp (funcName d) cg sii (funcRhs d) }

wrapExp :: Id -> DepGraph -> StrictIntInfo -> Exp -> Exp
wrapExp f cg sii (Fun g)
  | isPrimId g = Fun g
  | or siArgs
 && f `notElem` depends cg g
 && g `elem` depends cg g = Fun (g ++ "'0")
  | otherwise = Fun g
  where siArgs = retrieve g sii
wrapExp f cg sii e = descend (wrapExp f cg sii) e
