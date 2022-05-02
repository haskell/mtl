{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Module: Control.Monad.Accum
-- Copyright: (C) Koz Ross 2022
-- License: BSD-3-Clause (see the LICENSE file)
-- Maintainer: koz.ross@retro-freedom.nz
-- Stability: Experimental
-- Portability: GHC only
--
-- [Computation type:] Accumulation (either append-only state, or writer with
-- the ability to read all previous input).
--
-- [Binding strategy:] Binding a function to a monadic value monoidally
-- accumulates the subcomputations (that is, using '<>').
--
-- [Useful for:] Logging, patch-style tracking.
--
-- [Zero and plus:] None.
--
-- [Example type:] @'Accum' w a@
module Control.Monad.Accum
  ( MonadAccum (..),
    looks,
  )
where

import Data.Kind (Type)

-- | The capability to accumulate. This can be seen in one of two ways:
--
-- * A 'MonadState' which can only append (using '<>'); or
-- * A 'MonadWriter' (limited to 'tell') with the ability to view the result of
-- all previous 'tell's.
--
-- = Laws
--
-- 'accum' should obey the following:
--
-- 1. @'accum' ('const' (x, 'mempty'))@ @=@ @'pure' x@
-- 2. @'accum' f '*>' 'accum' g@ @=@
-- @'accum' '$' \acc -> let (_, v) = f acc
--                          (res, w) = g (acc '<>' v) in (res, v '<>' w)@
--
-- If you choose to define 'look' or 'add', their definitions must obey the
-- following laws. These are also their default definitions:
--
-- 1. @'look'@ @=@ @'accum' '$' \acc -> (acc, mempty)@
-- 2. @'add' x@ @=@ @'accum' '$' \acc -> ('()', x)@
--
-- All of the following are consequences of these definitions:
--
-- 1. @'look' '*>' 'look'@ @=@ @'look'@
-- 2. @'add' 'mempty'@ @=@ @'pure' '()'@
-- 3. @'add' x '*>' 'add' y@ @=@ @'add' (x '<>' y)@
-- 4. @'add' x '*>' 'look'@ @=@ @'look' '>>=' \w -> 'add' x '$>' w '<>' x@
--
-- @since 2.3
class (Monoid w, Monad m) => MonadAccum w m | m -> w where
  -- | Retrieve the accumulated result so far.
  look :: m w
  look = accum (,mempty)

  -- | Append a value to the result.
  add :: w -> m ()
  add x = accum $ const ((), x)

  -- | Embed a simple accumulation action into the monad.
  accum :: (w -> (a, w)) -> m a

  {-# MINIMAL accum #-}

-- | Retrieve a function of the accumulated value.
--
-- @since 2.3
looks ::
  forall (a :: Type) (m :: Type -> Type) (w :: Type).
  (MonadAccum w m) =>
  (w -> a) ->
  m a
looks f = f <$> look
