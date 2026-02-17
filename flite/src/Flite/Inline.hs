module Flite.Inline (InlineFlag(..), inline, inlineTop) where

import           Control.Monad
import           Flite.ConcatApp
import           Flite.Dependency
import           Flite.Descend
import           Flite.Fresh
import           Flite.Let
import           Flite.Syntax
import           Flite.Traversals

data InlineFlag = NoInline | InlineAll | InlineSmall Int

checkInline :: InlineFlag -> Int -> Bool
checkInline NoInline n            = False
checkInline InlineAll n           = True
checkInline (InlineSmall bound) n = n <= bound

inlineTop :: InlineFlag -> Prog -> Fresh Prog
inlineTop NoInline p = return p
inlineTop i p = inline i p
            >>= inlineLinearLet
            >>= inlineSimpleLet

-- In-line saturated applications of small, non-recursive functions
inline :: InlineFlag -> Prog -> Fresh Prog
inline i p = onExpM inl p
  where
    cg = closure (callGraph p)
    inl (Fun f)
      | f `notElem` depends cg f =
        case lookupFuncs f p of
          Func f [] rhs:_ | checkInline i (numApps rhs) -> inl rhs
          _                                             -> return (Fun f)
    inl (App (Fun f) (a:es)) -- | Never try to inline in the first argument of a
                             -- seq/par call... these always need to be heap
                             -- pointers.
      | isParSeq f = do
          es' <- mapM inl es
          pure $ App (Fun f) (a:es')
    inl (App (Fun f) es)
      | f `notElem` depends cg f =
        case lookupFuncs f p of
          Func f args rhs:_
            | length args <= length es
           && checkInline i (numApps rhs) ->
                do let vs = map (\(Var v) -> v) args
                   -- ws <- mapM (\_ -> fresh) vs
                   -- let rhs' = substMany rhs (zip (map Var ws) vs)
                   (ws, rhs') <- freshBody (vs, rhs)
                   inl (mkApp (mkLet (zip ws es) rhs') (drop (length vs) es))
          _ -> fmap (mkApp (Fun f)) (mapM inl es)
    inl e = descendM inl e

mkApp f [] = f
mkApp f es = App f es

mkLet [] e = e
mkLet bs e = Let bs e

numApps (App f xs)  = 1 + sum (map numApps (f:xs))
numApps (Let bs e)  = sum (map numApps (e:map snd bs))
numApps (Case e as) = max 1 (numApps e) + sum (map (numApps . snd) as)
numApps e           = 0;
