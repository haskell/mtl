{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Accum.Class
-- Copyright   :  (c) Manuel Bärenz,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-param classes, functional dependencies)
--
-- The MonadAccum class.
--
--      Inspired by the paper
--      /Functional Programming with Overloading and Higher-Order Polymorphism/,
--        Mark P Jones (<http://web.cecs.pdx.edu/~mpj/pubs/springschool.html>)
--          Advanced School of Functional Programming, 1995.
-----------------------------------------------------------------------------

module Control.Monad.Accum.Class (
    MonadAccum(..),
    looks,
  ) where

import Control.Monad.Trans.Except as Except
import Control.Monad.Trans.Identity as Identity
import Control.Monad.Trans.Maybe as Maybe
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.RWS.Lazy as Lazy (
        RWST, writer, tell, listen, pass)
import qualified Control.Monad.Trans.RWS.Strict as Strict (
        RWST, writer, tell, listen, pass)
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy (
        WriterT, writer, tell, listen, pass)
import qualified Control.Monad.Trans.Writer.Strict as Strict (
        WriterT, writer, tell, listen, pass)

#if MIN_VERSION_transformers(0,5,3)
import qualified Control.Monad.Trans.Accum as Accum
#endif

#if MIN_VERSION_transformers(0,5,6)
import qualified Control.Monad.Trans.RWS.CPS as CPS (
        RWST, writer, tell, listen, pass)
import qualified Control.Monad.Trans.Writer.CPS as CPS (
        WriterT, writer, tell, listen, pass)
#endif

import Control.Monad.Trans.Class (lift)
import Control.Monad
import Data.Monoid

class (Monoid w, Monad m) => MonadAccum w m | m -> w where
#if __GLASGOW_HASKELL__ >= 707
    {-# MINIMAL (accum | look, add) #-}
#endif
    -- | 'accum' embeds an accumulation action.
    accum :: (w -> (a, w)) -> m a
    accum f = do
      w <- look
      let ~(a, w') = f w
      add w'
      return a

    -- | @'add' w@ is an action that adds @w@ to the log.
    add :: w -> m ()
    add w = accum $ const ((),w)

    -- | 'look' returns the log accumulated so far.
    look :: m w
    look = accum (, mempty)

-- | @'looks' f@ returns 'f' applied to the log accumulated so far.
--
-- * @'looks' f = f <$> 'look'@
looks :: MonadAccum w m => (w -> a) -> m a
looks f = f <$> look

#if MIN_VERSION_transformers(0,5,3)
instance (Monoid w, Monad m) => MonadAccum w (Accum.AccumT w m) where
    accum = Accum.accum
    add = Accum.add
    look = Accum.look
#endif

-- ---------------------------------------------------------------------------
-- Instances for other mtl transformers
--
-- All of these instances need UndecidableInstances,
-- because they do not satisfy the coverage condition.

-- | @since 2.3
instance MonadAccum w m => MonadAccum w (ExceptT e m) where
    accum = lift . accum
    add = lift . add
    look = lift look

-- | @since 2.3
instance MonadAccum w m => MonadAccum w (IdentityT m) where
    accum = lift . accum
    add = lift . add
    look = lift look

-- | @since 2.3
instance MonadAccum w m => MonadAccum w (MaybeT m) where
    accum = lift . accum
    add = lift . add
    look = lift look

-- | @since 2.3
instance MonadAccum w m => MonadAccum w (ReaderT r m) where
    accum = lift . accum
    add = lift . add
    look = lift look

-- | @since 2.3
instance MonadAccum w m => MonadAccum w (Lazy.StateT s m) where
    accum = lift . accum
    add = lift . add
    look = lift look

-- | @since 2.3
instance MonadAccum w m => MonadAccum w (Strict.StateT s m) where
    accum = lift . accum
    add = lift . add
    look = lift look

#if MIN_VERSION_transformers(0,5,6)
-- | @since 2.3
instance MonadAccum w m => MonadAccum w (CPS.WriterT w m) where
    accum = lift . accum
    add = lift . add
    look = lift look
#endif

-- | @since 2.3
instance MonadAccum w m => MonadAccum w (Lazy.WriterT w m) where
    accum = lift . accum
    add = lift . add
    look = lift look

-- | @since 2.3
instance MonadAccum w m => MonadAccum w (Strict.WriterT w m) where
    accum = lift . accum
    add = lift . add
    look = lift look


#if MIN_VERSION_transformers(0,5,6)
-- | @since 2.3
instance MonadAccum w m => MonadAccum w (CPS.RWST r w s m) where
    accum = lift . accum
    add = lift . add
    look = lift look
#endif

-- | @since 2.3
instance MonadAccum w m => MonadAccum w (Lazy.RWST r w s m) where
    accum = lift . accum
    add = lift . add
    look = lift look

-- | @since 2.3
instance MonadAccum w m => MonadAccum w (Strict.RWST r w s m) where
    accum = lift . accum
    add = lift . add
    look = lift look
