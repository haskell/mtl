{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Accum (
  accumConstLaw,
  accumThenLaw
  ) where

import Data.Kind (Type)
import Test.QuickCheck (Property)

accumConstLaw :: forall (m :: Type -> Type) .
  (forall (a :: Type) . m a -> a) -> Property
accumConstLaw lower = _

accumThenLaw :: forall (m :: Type -> Type) . 
  (forall (a :: Type) . m a -> a) -> Property
accumThenLaw lower = _
