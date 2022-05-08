{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Module: Control.Monad.List
-- Copyright: (C) Koz Ross 2022
-- License: BSD-3-Clause (see the LICENSE file)
-- Maintainer: koz.ross@retro-freedom.nz
-- Stability: Experimental
-- Portability: GHC only
module Control.Monad.List
  ( -- * Monad transformer
    ListT (..),
  )
where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (ap)
import Control.Monad.Accum (LiftingAccum (LiftingAccum), MonadAccum)
import Control.Monad.Cont (MonadCont (callCC))
import Control.Monad.Except (MonadError (catchError, throwError))
import Control.Monad.Fail (MonadFail (fail))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask, local))
import Control.Monad.Select (LiftingSelect (LiftingSelect), MonadSelect)
import Control.Monad.State (MonadState (get, put))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Writer (MonadWriter (listen, pass, tell))
import Control.Monad.Zip (MonadZip (mzipWith))
import Data.Bifunctor (bimap)
import Data.Functor.Identity (Identity (Identity))
import Data.Kind (Type)
import GHC.Exts (IsList (Item, fromList, toList))

-- | @since 2.3.1
newtype ListT (m :: Type -> Type) (a :: Type) = ListT
  { -- | @since 2.3.1
    runListT :: m (Maybe (a, ListT m a))
  }

-- | @since 2.3.1
instance (Functor m) => Functor (ListT m) where
  fmap f = ListT . fmap (fmap (bimap f (fmap f))) . runListT

-- | @since 2.3.1
instance IsList (ListT Identity a) where
  type Item (ListT Identity a) = a
  fromList = \case
    [] -> emptyListT
    (x : xs) -> ListT . Identity . Just $ (x, fromList xs)
  toList (ListT (Identity xs)) = case xs of
    Nothing -> []
    Just (x, xs') -> x : toList xs'

-- | @since 2.3.1
instance (Monad m) => Applicative (ListT m) where
  pure x = ListT . pure . Just $ (x, ListT . pure $ Nothing)
  (<*>) = ap

-- | @since 2.3.1
instance (Monad m) => Monad (ListT m) where
  ListT xs >>= f = ListT $ do
    res <- xs
    case res of
      Nothing -> pure Nothing
      Just (x, xs') -> do
        let ell = f x
        let ell' = xs' >>= f
        let ListT concatenated = concatListT ell ell'
        concatenated

-- | @since 2.3.1
instance (Monad m) => Alternative (ListT m) where
  empty = emptyListT
  (<|>) = concatListT

-- | @since 2.3.1
instance (Monad m) => Semigroup (ListT m a) where
  (<>) = (<|>)

-- | @since 2.3.1
instance (Monad m) => Monoid (ListT m a) where
  mempty = empty

-- | @since 2.3.1
deriving stock instance
  (forall (b :: Type). Eq b => Eq (m b), Eq a) =>
  Eq (ListT m a)

-- | @since 2.3.1
deriving stock instance
  ( forall (b :: Type). Ord b => Ord (m b),
    forall (b :: Type). Eq b => Eq (m b),
    Ord a
  ) =>
  Ord (ListT m a)

-- | @since 2.3.1
deriving stock instance
  (forall (b :: Type). Show b => Show (m b), Show a) =>
  Show (ListT m a)

-- | @since 2.3.1
deriving stock instance
  (forall (b :: Type). Read b => Read (m b), Read a) =>
  Read (ListT m a)

-- | @since 2.3.1
instance MonadTrans ListT where
  lift comp = ListT $ do
    x <- comp
    pure . Just $ (x, emptyListT)

-- | @since 2.3.1
instance (Monad m) => MonadZip (ListT m) where
  mzipWith f (ListT xs) (ListT ys) = ListT $ do
    leftComp <- xs
    case leftComp of
      Nothing -> pure Nothing
      Just (x, xs') -> do
        rightComp <- ys
        case rightComp of
          Nothing -> pure Nothing
          Just (y, ys') -> pure . Just $ (f x y, mzipWith f xs' ys')

-- | @since 2.3.1
instance (Monad m) => MonadFail (ListT m) where
  fail _ = emptyListT

-- | @since 2.3.1
instance (MonadIO m) => MonadIO (ListT m) where
  liftIO comp = ListT $ do
    x <- liftIO comp
    pure . Just $ (x, emptyListT)

-- | Each non-deterministic computation maintains its own accumulated value. In
-- particular, if a non-deterministic branch fails, its accumulated value
-- disappears.
--
-- @since 2.3.1
deriving via
  (LiftingAccum ListT m)
  instance
    (MonadAccum w m, Monoid w) =>
    MonadAccum w (ListT m)

-- | Each non-deterministic success, along with /all/ other successes, gets ranked by
-- a dedicated \'ranking\' function based on that success, which determines which
-- success(es) get chosen.
--
-- @since 2.3.1
deriving via
  (LiftingSelect ListT m)
  instance
    (MonadSelect r m) =>
    MonadSelect r (ListT m)

-- | Each non-deterministic computation gets its own read-only @r@.
--
-- @since 2.3.1
instance (MonadReader r m) => MonadReader r (ListT m) where
  ask = lift ask
  local f (ListT comp) = ListT $ local f comp

-- | Each non-deterministic computation gets its own state, which it, and it
-- /alone/, can modify (and read).
--
-- @since 2.3.1
instance (MonadState s m) => MonadState s (ListT m) where
  get = lift get
  put x = lift (put x)

-- | Each non-deterministic computation gets its own writer, which it, and it
-- /alone/, can contribute to.
--
-- @since 2.3.1
instance (MonadWriter w m) => MonadWriter w (ListT m) where
  tell w = lift (tell w)
  listen (ListT comp) = ListT $ do
    (res, acc) <- listen comp
    case res of
      Nothing -> pure Nothing
      Just (x, xs) -> do
        let rest = listen xs
        pure . Just $ ((x, acc), rest)
  pass (ListT xs) = ListT $ do
    res <- xs
    case res of
      Nothing -> pure Nothing
      Just (passable, xs') -> do
        x <- pass . pure $ passable
        let rest = pass xs'
        pure . Just $ (x, rest)

-- | Throwing an error \'hard aborts\' the non-determinism; no future
-- computations will produce /any/ results. Catching such a \'hard abort\'
-- allows you to \'patch in\' new non-deterministic computations, but these will
-- completely replace whatever you would have received otherwise.
--
-- @since 2.3.1
instance (MonadError e m) => MonadError e (ListT m) where
  throwError e = lift (throwError e)
  catchError (ListT comp) handle = ListT $ catchError comp (runListT . handle)

-- | @since 2.3.1
instance (MonadCont m) => MonadCont (ListT m) where
  callCC k = ListT $
    callCC $ \k' ->
      runListT . k $ \x -> ListT . k' . Just $ (x, emptyListT)

-- Helpers

emptyListT ::
  forall (a :: Type) (m :: Type -> Type).
  (Applicative m) =>
  ListT m a
emptyListT = ListT . pure $ Nothing

concatListT ::
  forall (a :: Type) (m :: Type -> Type).
  (Monad m) =>
  ListT m a ->
  ListT m a ->
  ListT m a
concatListT (ListT xs) rights@(ListT ys) = ListT $ do
  leftMay <- xs
  case leftMay of
    Nothing -> ys
    Just (x, xs') -> pure . Just $ (x, concatListT xs' rights)
