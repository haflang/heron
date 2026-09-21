{-# LANGUAGE GADTs #-}
module Flite.Fresh where

import           Control.Applicative (Applicative (..))
import           Control.Monad       (ap, liftM)

data Fresh a where
  Fresh :: {runFresh :: String -> Int -> (Int, a)} -> Fresh a

instance Monad Fresh where
  return a = Fresh (\s i -> (i, a))
  m >>= f  = Fresh (\s i -> case runFresh m s i of
                              (j, a) -> runFresh (f a) s j)

instance Functor Fresh where
  fmap = liftM

instance Applicative Fresh where
  pure  = return
  (<*>) = ap

fresh :: Fresh String
fresh = Fresh (\s i -> (i+1, s ++ show i))
