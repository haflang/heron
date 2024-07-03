{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE TypeFamilyDependencies #-}

{-| Shared types for Heron Core components -}
module Heron.Core.Types where

-- | Silly class to enable nice (duplicated) accessor names for stacks, parallel
--   stacks, and heap.
class SizedRead a where
  type SizedAddr a
  type SizedData a
  size  :: a -> SizedAddr a
  read  :: a -> SizedData a
