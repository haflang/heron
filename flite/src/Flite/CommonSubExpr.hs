module Flite.CommonSubExpr(elimCommonSubExpr) where

import Flite.Dependency
import Flite.Syntax
import Flite.Traversals
import Flite.Descend
import Flite.Fresh
import Flite.Pretty
import Data.List
import Data.Maybe
import Control.Applicative

{-

Let's see what we can catch if we check for common subexpressions after
flattening an F-lite source.

If we ignore any partial applications, this becomes:

  1) an equivalence check between bindings, since there are no subexpressions.
  This is modulo neat tricks such as lifting let bindings up from case
  alternatives, etc. (We can't easily do that after case elimination anyway)

  2) a check for equivalent subexpressions in the spine? It often costs no more
  to instantiate something on the spine _and_ the heap, so let's ignore that.

Do 1) until we hit a fixed point.
<<<<<<< HEAD
Probably best to start with some toy examples.
=======
>>>>>>> 8ad5b30 (Update hdl benchmarks)

-}

duplicateBinding :: [(Id, App)] -> Id -> Maybe (Id, Id)
duplicateBinding bs b = annotate <$> dup
  where
    bs' = filter ((/= b) . fst) bs
    e   = fromJust $ lookup b bs
    dup = listToMaybe $ filter ((e ==) . snd) bs'
    annotate (d,_) = (b,d)

elimCommonSubExpr :: [(Id, App)] -> [(Id, App)]
elimCommonSubExpr bs =
  case duplicate of
    Nothing -> bs
    Just (orig,dup) ->
      elimCommonSubExpr . map (substBnd orig dup) $
      filter ((/= dup) . fst) bs
  where
    duplicate = foldl findDuplicate Nothing (map fst $ tail bs)
    findDuplicate acc b = acc <|> duplicateBinding bs b
    substBnd orig dup (n,b) = (n, map (subst (Var orig) dup) b)
