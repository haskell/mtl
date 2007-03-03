{-# OPTIONS -fallow-undecidable-instances #-}
-- Search for -fallow-undecidable-instances to see why this is needed

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Cont
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-parameter type classes)
--
-- Continuation monads.
--
-----------------------------------------------------------------------------

module Control.Monad.Cont (
    module Control.Monad.Cont.Class,
    Cont(..),
    mapCont,
    withCont,
    ContT(..),
    mapContT,
    withContT,
    module Control.Monad,
    module Control.Monad.Trans,
  ) where

import Control.Monad
import Control.Monad.Cont.Class
import Control.Monad.RWS.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans

-- ---------------------------------------------------------------------------
-- Our parameterizable continuation monad

newtype Cont r a = Cont { runCont :: (a -> r) -> r }

mapCont :: (r -> r) -> Cont r a -> Cont r a
mapCont f m = Cont $ f . runCont m

withCont :: ((b -> r) -> (a -> r)) -> Cont r a -> Cont r b
withCont f m = Cont $ runCont m . f

instance Functor (Cont r) where
    fmap f m = Cont $ \c -> runCont m (c . f)

instance Monad (Cont r) where
    return a = Cont ($ a)
    m >>= k  = Cont $ \c -> runCont m $ \a -> runCont (k a) c

instance MonadCont (Cont r) where
    callCC f = Cont $ \c -> runCont (f (\a -> Cont $ \_ -> c a)) c

-- ---------------------------------------------------------------------------
-- Our parameterizable continuation monad, with an inner monad

newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }

mapContT :: (m r -> m r) -> ContT r m a -> ContT r m a
mapContT f m = ContT $ f . runContT m

withContT :: ((b -> m r) -> (a -> m r)) -> ContT r m a -> ContT r m b
withContT f m = ContT $ runContT m . f

instance (Monad m) => Functor (ContT r m) where
    fmap f m = ContT $ \c -> runContT m (c . f)

instance (Monad m) => Monad (ContT r m) where
    return a = ContT ($ a)
    m >>= k  = ContT $ \c -> runContT m (\a -> runContT (k a) c)

instance (Monad m) => MonadCont (ContT r m) where
    callCC f = ContT $ \c -> runContT (f (\a -> ContT $ \_ -> c a)) c

-- ---------------------------------------------------------------------------
-- Instances for other mtl transformers

instance MonadTrans (ContT r) where
    lift m = ContT (m >>=)

instance (MonadIO m) => MonadIO (ContT r m) where
    liftIO = lift . liftIO

-- Needs -fallow-undecidable-instances
instance (MonadReader r' m) => MonadReader r' (ContT r m) where
    ask       = lift ask
    local f m = ContT $ \c -> do
        r <- ask
        local f (runContT m (local (const r) . c))

-- Needs -fallow-undecidable-instances
instance (MonadState s m) => MonadState s (ContT r m) where
    get = lift get
    put = lift . put

