{-# LANGUAGE CPP #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
-- Search for UndecidableInstances to see why this is needed

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Accum.Class
-- Copyright   :  ???
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-param classes, functional dependencies)
--
-- MonadAccum class.
--

-----------------------------------------------------------------------------

module Control.Monad.Accum.Class (
    MonadAccum(..),
    looks
  ) where

import Control.Monad.Trans.Cont
import Control.Monad.Trans.Error
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.Accum as A
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS (RWST, get, put, state)
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS (RWST, get, put, state)
import qualified Control.Monad.Trans.State.Lazy as Lazy (StateT, get, put, state)
import qualified Control.Monad.Trans.State.Strict as Strict (StateT, get, put, state)
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict

import Control.Monad.Trans.Class (lift)
import Control.Monad
import Data.Monoid

-- ---------------------------------------------------------------------------

-- | Minimal definition is either both of @get@ and @put@ or just @state@
class (Monoid w, Monad m) => MonadAccum w m | m -> w where
    -- | @'add' w@ is an action that produces the output @w@.
    add :: w -> m ()

    -- | 'look' is an action that fetchs all the previously accumulated
    -- output.
    look :: m w

#if __GLASGOW_HASKELL__ >= 707
    {-# MINIMAL add, look #-}
#endif

-- | 'looks' is an action that retrieves a function of the previously
-- accumulated output.
looks :: MonadAccum w m => (w -> a) -> m a
looks f = fmap f look

instance (Monoid w, Monad m) => MonadAccum w (A.AccumT w m) where
    add  = A.add
    look = A.look

-- ---------------------------------------------------------------------------
-- Instances for other mtl transformers
--
-- All of these instances need UndecidableInstances,
-- because they do not satisfy the coverage condition.

instance MonadAccum w m => MonadAccum w (ContT r m) where
    add  = lift . add
    look = lift look

instance (Error e, MonadAccum w m) => MonadAccum w (ErrorT e m) where
    add  = lift . add
    look = lift look

instance MonadAccum w m => MonadAccum w (ExceptT e m) where
    add  = lift . add
    look = lift look

instance MonadAccum w m => MonadAccum w (IdentityT m) where
    add  = lift . add
    look = lift look

instance MonadAccum w m => MonadAccum w (ListT m) where
    add  = lift . add
    look = lift look

instance MonadAccum w m => MonadAccum w (MaybeT m) where
    add  = lift . add
    look = lift look

instance MonadAccum w m => MonadAccum w (ReaderT r m) where
    add  = lift . add
    look = lift look

instance MonadAccum w m => MonadAccum w (Lazy.StateT s m) where
    add  = lift . add
    look = lift look

instance MonadAccum w m => MonadAccum w (Strict.StateT s m) where
    add  = lift . add
    look = lift look

instance (Monoid v, MonadAccum w m) => MonadAccum w (Lazy.WriterT v m) where
    add  = lift . add
    look = lift look

instance (Monoid v, MonadAccum w m) => MonadAccum w (Strict.WriterT v m) where
    add  = lift . add
    look = lift look

