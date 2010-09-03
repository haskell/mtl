{-# LANGUAGE UndecidableInstances #-}
-- Search for UndecidableInstances to see why this is needed

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Writer.Lazy
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-param classes, functional dependencies)
--
-- Lazy writer monads.
--
--      Inspired by the paper
--      /Functional Programming with Overloading and
--          Higher-Order Polymorphism/,
--        Mark P Jones (<http://web.cecs.pdx.edu/~mpj/pubs/springschool.html>)
--          Advanced School of Functional Programming, 1995.
-----------------------------------------------------------------------------

module Control.Monad.Writer.Lazy (
    module Control.Monad.Writer.Class,
    Writer(..),
    execWriter,
    mapWriter,
    WriterT(..),
    execWriterT,
    mapWriterT,
    module Control.Monad,
    module Control.Monad.Fix,
    module Control.Monad.Trans,
    module Data.Monoid,
  ) where

import Control.Monad
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import Control.Monad.Fix
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Writer.Class
import Data.Monoid

-- ---------------------------------------------------------------------------
-- Our parameterizable writer monad

newtype Writer w a = Writer { runWriter :: (a, w) }

execWriter :: Writer w a -> w
execWriter m = snd (runWriter m)

mapWriter :: ((a, w) -> (b, w')) -> Writer w a -> Writer w' b
mapWriter f m = Writer $ f (runWriter m)

instance Functor (Writer w) where
    fmap f m = Writer $ let (a, w) = runWriter m in (f a, w)

instance (Monoid w) => Monad (Writer w) where
    return a = Writer (a, mempty)
    m >>= k  = Writer $ let
        (a, w)  = runWriter m
        (b, w') = runWriter (k a)
        in (b, w `mappend` w')

instance (Monoid w) => MonadFix (Writer w) where
    mfix m = Writer $ let (a, w) = runWriter (m a) in (a, w)

instance (Monoid w) => MonadWriter w (Writer w) where
    tell   w = Writer ((), w)
    listen m = Writer $ let (a, w) = runWriter m in ((a, w), w)
    pass   m = Writer $ let ((a, f), w) = runWriter m in (a, f w)

-- ---------------------------------------------------------------------------
-- Our parameterizable writer monad, with an inner monad

newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }

execWriterT :: Monad m => WriterT w m a -> m w
execWriterT m = do
    ~(_, w) <- runWriterT m
    return w

mapWriterT :: (m (a, w) -> n (b, w')) -> WriterT w m a -> WriterT w' n b
mapWriterT f m = WriterT $ f (runWriterT m)

instance (Monad m) => Functor (WriterT w m) where
    fmap f m = WriterT $ do
        ~(a, w) <- runWriterT m
        return (f a, w)

instance (Monoid w, Monad m) => Monad (WriterT w m) where
    return a = WriterT $ return (a, mempty)
    m >>= k  = WriterT $ do
        ~(a, w)  <- runWriterT m
        ~(b, w') <- runWriterT (k a)
        return (b, w `mappend` w')
    fail msg = WriterT $ fail msg

instance (Monoid w, MonadPlus m) => MonadPlus (WriterT w m) where
    mzero       = WriterT mzero
    m `mplus` n = WriterT $ runWriterT m `mplus` runWriterT n

instance (Monoid w, MonadFix m) => MonadFix (WriterT w m) where
    mfix m = WriterT $ mfix $ \ ~(a, _) -> runWriterT (m a)

instance (Monoid w, Monad m) => MonadWriter w (WriterT w m) where
    tell   w = WriterT $ return ((), w)
    listen m = WriterT $ do
        ~(a, w) <- runWriterT m
        return ((a, w), w)
    pass   m = WriterT $ do
        ~((a, f), w) <- runWriterT m
        return (a, f w)

-- ---------------------------------------------------------------------------
-- Instances for other mtl transformers

instance (Monoid w) => MonadTrans (WriterT w) where
    lift m = WriterT $ do
        a <- m
        return (a, mempty)

instance (Monoid w, MonadIO m) => MonadIO (WriterT w m) where
    liftIO = lift . liftIO

instance (Monoid w, MonadCont m) => MonadCont (WriterT w m) where
    callCC f = WriterT $
        callCC $ \c ->
        runWriterT (f (\a -> WriterT $ c (a, mempty)))

instance (Monoid w, MonadError e m) => MonadError e (WriterT w m) where
    throwError       = lift . throwError
    m `catchError` h = WriterT $ runWriterT m
        `catchError` \e -> runWriterT (h e)

-- This instance needs UndecidableInstances, because
-- it does not satisfy the coverage condition
instance (Monoid w, MonadReader r m) => MonadReader r (WriterT w m) where
    ask       = lift ask
    local f m = WriterT $ local f (runWriterT m)

-- Needs UndecidableInstances
instance (Monoid w, MonadState s m) => MonadState s (WriterT w m) where
    get = lift get
    put = lift . put

