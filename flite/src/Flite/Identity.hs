module Flite.Identity where

import           Control.Applicative (Applicative (..))
import           Control.Monad       (ap, liftM)

newtype Identity a = I { runIdentity :: a }

instance Monad Identity where
  return = I
  I a >>= f = f a

instance Functor Identity where
  fmap = liftM

instance Applicative Identity where
  pure  = return
  (<*>) = ap
