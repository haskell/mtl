{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
-- Later GHCs infer DerivingVia as not Safe
-- We just downgrade to Trustworthy and go fish
{-# OPTIONS_GHC -Wno-trustworthy-safe #-}

-- | Module: Control.Monad.Accum
-- Copyright: (C) Koz Ross 2022, Manuel Bärenz 2021
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
-- [Example type:] @'Control.Monad.Trans.Accum.Accum' w a@
--
-- = A note on commutativity
--
-- Some effects are /commutative/: it doesn't matter which you resolve first, as
-- all possible orderings of commutative effects are isomorphic. Consider, for
-- example, the reader and state effects, as exemplified by 'ReaderT' and
-- 'StrictState.StateT' respectively. If we have
-- @'ReaderT' r ('StrictState.State' s) a@, this is
-- effectively @r -> 'StrictState.State' s a ~ r -> s -> (a, s)@; if we instead have
-- @'StrictState.StateT' s ('Control.Monad.Trans.Reader.Reader' r) a@, this is effectively
-- @s -> 'Control.Monad.Trans.Reader' r (a, s) ~ s -> r -> (a, s)@. Since we
-- can always reorder function arguments (for example, using 'flip', as in
-- this case) without changing the result, these are
-- isomorphic, showing that reader and state are /commutative/, or, more
-- precisely, /commute with each other/.
--
-- However, this isn't generally the case. Consider instead the error and state
-- effects, as exemplified by 'MaybeT' and 'StrictState.StateT' respectively.
-- If we have @'MaybeT' ('Control.Monad.Trans.State.Strict.State' s) a@, this
-- is effectively @'State' s ('Maybe' a) ~ s -> ('Maybe' a, s)@: put simply,
-- the error can occur only in the /result/, but
-- not the state, which always \'survives\'. On the other hand, if we have
-- @'StrictState.StateT' s 'Maybe' a@, this is instead @s -> 'Maybe' (a, s)@: here,
-- if we error, we lose /both/ the state and the result! Thus, error and state effects
-- do /not/ commute with each other.
--
-- As the MTL is capability-based, we support any ordering of non-commutative
-- effects on an equal footing. Indeed, if you wish to use
-- 'Control.Monad.State.Class.MonadState', for
-- example, whether your final monadic stack ends up being @'MaybeT'
-- ('Control.Monad.Trans.State.Strict.State' s)
-- a@, @'StrictState.StateT' s 'Maybe' a@, or anything else, you will be able to write your
-- desired code without having to consider such differences. However, the way we
-- /implement/ these capabilities for any given transformer (or rather, any
-- given transformed stack) /is/ affected by this ordering unless the effects in
-- question are commutative.
--
-- We note in this module which effects the accumulation effect does and doesn't
-- commute with; we also note on implementations with non-commutative
-- transformers what the outcome will be. Note that, depending on how the
-- \'inner monad\' is structured, this may be more complex than we note: we
-- describe only what impact the \'outer effect\' has, not what else might be in
-- the stack.
--
-- = Commutativity of accumulation
--
-- The accumulation effect commutes with the identity effect ('IdentityT'),
-- reader, writer or state effects ('ReaderT', 'StrictWriter.WriterT', 'StrictState.StateT' and any
-- combination, including 'StrictRWS.RWST' for example) and with itself. It does /not/
-- commute with anything else.
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
-- * A 'Control.Monad.State.Class.MonadState' which can only append (using '<>'); or
-- * A 'Control.Monad.Writer.Class.MonadWriter' (limited to
-- 'Control.Monad.Writer.Class.tell') with the ability to view the result of all previous
-- 'Control.Monad.Writer.Class.tell's.
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
-- 2. @'add' 'mempty'@ @=@ @'pure' ()@
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

-- | The accumulated value \'survives\' an error: even if the
-- computation fails to deliver a result, we still have an accumulated value.
--
-- @since 2.3
deriving via
  (LiftingAccum MaybeT m)
  instance
    (MonadAccum w m) =>
    MonadAccum w (MaybeT m)

-- | The continuation can see, and interact with, the accumulated value.
--
-- @since 2.3
deriving via
  (LiftingAccum (ContT r) m)
  instance
    (MonadAccum w m) =>
    MonadAccum w (ContT r m)

-- | The accumulated value \'survives\' an exception: even if the computation
-- fails to deliver a result, we still have an accumulated value.
--
-- @since 2.3
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

-- | The \'ranking\' function gains the ability to accumulate @w@s each time it
-- is called. The final result will include the entire log of all such calls.
--
-- @since 2.3
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
