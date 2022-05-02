{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

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
  ( -- * Type class
    MonadAccum (..),

    -- * Lifting helper type
    LiftingAccum (..),

    -- * Other functions
    looks,
  )
where

import Control.Monad.Trans.Accum (AccumT)
import qualified Control.Monad.Trans.Accum as Accum
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.Maybe (MaybeT)
import qualified Control.Monad.Trans.RWS.CPS as CPSRWS
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Select (SelectT)
import qualified Control.Monad.Trans.State.Lazy as LazyState
import qualified Control.Monad.Trans.State.Strict as StrictState
import qualified Control.Monad.Trans.Writer.CPS as CPSWriter
import qualified Control.Monad.Trans.Writer.Lazy as LazyWriter
import qualified Control.Monad.Trans.Writer.Strict as StrictWriter
import Data.Functor (($>))
import Data.Functor.Identity (Identity)
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
-- If you choose to define 'look' and 'add' instead, their definitions must obey
-- the following:
--
-- 1. @'look' '*>' 'look'@ @=@ @'look'@
-- 2. @'add' 'mempty'@ @=@ @'pure' '()'@
-- 3. @'add' x '*>' 'add' y@ @=@ @'add' (x '<>' y)@
-- 4. @'add' x '*>' 'look'@ @=@ @'look' '>>=' \w -> 'add' x '$>' w '<>' x@
--
-- If you want to define both, the relationship between them is as follows.
-- These are also the default definitions.
--
-- 1. @'look'@ @=@ @'accum' '$' \acc -> (acc, mempty)@
-- 2. @'add' x@ @=@ @'accum' '$' \acc -> ('()', x)@
-- 3. @'accum' f@ @=@ @'look' >>= \acc -> let (res, v) = f acc in 'add' v '$>' res@
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
  accum f = look >>= \acc -> let (res, v) = f acc in add v $> res

  {-# MINIMAL accum | look, add #-}

-- | @since 2.3
instance (Monoid w) => MonadAccum w (AccumT w Identity) where
  look = Accum.look
  add = Accum.add
  accum = Accum.accum

-- | @since 2.3
deriving via
  (LiftingAccum MaybeT m)
  instance
    (MonadAccum w m) =>
    MonadAccum w (MaybeT m)

-- | @since 2.3
deriving via
  (LiftingAccum (ContT r) m)
  instance
    (MonadAccum w m) =>
    MonadAccum w (ContT r m)

-- | @since 2.3
deriving via
  (LiftingAccum (ExceptT e) m)
  instance
    (MonadAccum w m) =>
    MonadAccum w (ExceptT e m)

-- | @since 2.3
deriving via
  (LiftingAccum IdentityT m)
  instance
    (MonadAccum w m) =>
    MonadAccum w (IdentityT m)

-- | @since 2.3
deriving via
  (LiftingAccum (CPSRWS.RWST r w s) m)
  instance
    (MonadAccum w' m) =>
    MonadAccum w' (CPSRWS.RWST r w s m)

-- | @since 2.3
deriving via
  (LiftingAccum (LazyRWS.RWST r w s) m)
  instance
    (MonadAccum w' m, Monoid w) =>
    MonadAccum w' (LazyRWS.RWST r w s m)

-- | @since 2.3
deriving via
  (LiftingAccum (StrictRWS.RWST r w s) m)
  instance
    (MonadAccum w' m, Monoid w) =>
    MonadAccum w' (StrictRWS.RWST r w s m)

-- | @since 2.3
deriving via
  (LiftingAccum (ReaderT r) m)
  instance
    (MonadAccum w m) =>
    MonadAccum w (ReaderT r m)

-- | @since 2.3
deriving via
  (LiftingAccum (SelectT r) m)
  instance
    (MonadAccum w m) =>
    MonadAccum w (SelectT r m)

-- | @since 2.3
deriving via
  (LiftingAccum (LazyState.StateT s) m)
  instance
    (MonadAccum w m) =>
    MonadAccum w (LazyState.StateT s m)

-- | @since 2.3
deriving via
  (LiftingAccum (StrictState.StateT s) m)
  instance
    (MonadAccum w m) =>
    MonadAccum w (StrictState.StateT s m)

-- | @since 2.3
deriving via
  (LiftingAccum (CPSWriter.WriterT w) m)
  instance
    (MonadAccum w' m) =>
    MonadAccum w' (CPSWriter.WriterT w m)

-- | @since 2.3
deriving via
  (LiftingAccum (LazyWriter.WriterT w) m)
  instance
    (MonadAccum w' m, Monoid w) =>
    MonadAccum w' (LazyWriter.WriterT w m)

-- | @since 2.3
deriving via
  (LiftingAccum (StrictWriter.WriterT w) m)
  instance
    (MonadAccum w' m, Monoid w) =>
    MonadAccum w' (StrictWriter.WriterT w m)

-- | A helper type to decrease boilerplate when defining new transformer
-- instances of 'MonadAccum'.
--
-- Most of the instances in this module are derived using this method; for
-- example, our instance of 'ExceptT' is derived as follows:
--
-- > deriving via (LiftingAccum (ExceptT e) m) instance (MonadAccum w m) =>
-- >  MonadAccum w (ExceptT e m)
--
-- @since 2.3
newtype LiftingAccum (t :: (Type -> Type) -> Type -> Type) (m :: Type -> Type) (a :: Type)
  = LiftingAccum (t m a)
  deriving
    ( -- | @since 2.3
      Functor,
      -- | @since 2.3
      Applicative,
      -- | @since 2.3
      Monad
    )
    via (t m)

-- | @since 2.3
instance (MonadTrans t, Monad (t m), MonadAccum w m) => MonadAccum w (LiftingAccum t m) where
  look = LiftingAccum . lift $ look
  add x = LiftingAccum . lift $ add x
  accum f = LiftingAccum . lift $ accum f

-- | Retrieve a function of the accumulated value.
--
-- @since 2.3
looks ::
  forall (a :: Type) (m :: Type -> Type) (w :: Type).
  (MonadAccum w m) =>
  (w -> a) ->
  m a
looks f = f <$> look
