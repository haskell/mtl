{-# OPTIONS -fallow-undecidable-instances #-}
-- Search for -fallow-undecidable-instances to see why this is needed
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Reader
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-param classes, functional dependencies)
--
-- Declaration of the MonadReader class
--
--      Inspired by the paper
--      /Functional Programming with Overloading and
--          Higher-Order Polymorphism/,
--        Mark P Jones (<http://www.cse.ogi.edu/~mpj/>)
--          Advanced School of Functional Programming, 1995.
-----------------------------------------------------------------------------

module Control.Monad.Reader (
    module Control.Monad.Reader.Class,
    Reader(..),
    mapReader,
    withReader,
    ReaderT(..),
    mapReaderT,
    withReaderT,
    module Control.Monad,
    module Control.Monad.Fix,
    module Control.Monad.Trans,
    ) where

import Control.Monad
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import Control.Monad.Fix
import Control.Monad.Instances ()
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Writer.Class

-- ----------------------------------------------------------------------------
-- The partially applied function type is a simple reader monad

instance MonadReader r ((->) r) where
    ask       = id
    local f m = m . f

-- ---------------------------------------------------------------------------
-- Our parameterizable reader monad

newtype Reader r a = Reader { runReader :: r -> a }

mapReader :: (a -> b) -> Reader r a -> Reader r b
mapReader f m = Reader $ f . runReader m

-- This is a more general version of local.

withReader :: (r' -> r) -> Reader r a -> Reader r' a
withReader f m = Reader $ runReader m . f

instance Functor (Reader r) where
    fmap f m = Reader $ \r -> f (runReader m r)

instance Monad (Reader r) where
    return a = Reader $ \_ -> a
    m >>= k  = Reader $ \r -> runReader (k (runReader m r)) r

instance MonadFix (Reader r) where
    mfix f = Reader $ \r -> let a = runReader (f a) r in a

instance MonadReader r (Reader r) where
    ask       = Reader id
    local f m = Reader $ runReader m . f

-- ---------------------------------------------------------------------------
-- Our parameterizable reader monad, with an inner monad

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

mapReaderT :: (m a -> n b) -> ReaderT w m a -> ReaderT w n b
mapReaderT f m = ReaderT $ f . runReaderT m

withReaderT :: (r' -> r) -> ReaderT r m a -> ReaderT r' m a
withReaderT f m = ReaderT $ runReaderT m . f

instance (Monad m) => Functor (ReaderT r m) where
    fmap f m = ReaderT $ \r -> do
        a <- runReaderT m r
        return (f a)

instance (Monad m) => Monad (ReaderT r m) where
    return a = ReaderT $ \_ -> return a
    m >>= k  = ReaderT $ \r -> do
        a <- runReaderT m r
        runReaderT (k a) r
    fail msg = ReaderT $ \_ -> fail msg

instance (MonadPlus m) => MonadPlus (ReaderT r m) where
    mzero       = ReaderT $ \_ -> mzero
    m `mplus` n = ReaderT $ \r -> runReaderT m r `mplus` runReaderT n r

instance (MonadFix m) => MonadFix (ReaderT r m) where
    mfix f = ReaderT $ \r -> mfix $ \a -> runReaderT (f a) r

instance (Monad m) => MonadReader r (ReaderT r m) where
    ask       = ReaderT return
    local f m = ReaderT $ \r -> runReaderT m (f r)

-- ---------------------------------------------------------------------------
-- Instances for other mtl transformers

instance MonadTrans (ReaderT r) where
    lift m = ReaderT $ \_ -> m

instance (MonadIO m) => MonadIO (ReaderT r m) where
    liftIO = lift . liftIO

instance (MonadCont m) => MonadCont (ReaderT r m) where
    callCC f = ReaderT $ \r ->
        callCC $ \c ->
        runReaderT (f (\a -> ReaderT $ \_ -> c a)) r

instance (MonadError e m) => MonadError e (ReaderT r m) where
    throwError       = lift . throwError
    m `catchError` h = ReaderT $ \r -> runReaderT m r
        `catchError` \e -> runReaderT (h e) r

-- Needs -fallow-undecidable-instances
instance (MonadState s m) => MonadState s (ReaderT r m) where
    get = lift get
    put = lift . put

-- This instance needs -fallow-undecidable-instances, because
-- it does not satisfy the coverage condition
instance (MonadWriter w m) => MonadWriter w (ReaderT r m) where
    tell     = lift . tell
    listen m = ReaderT $ \w -> listen (runReaderT m w)
    pass   m = ReaderT $ \w -> pass   (runReaderT m w)

