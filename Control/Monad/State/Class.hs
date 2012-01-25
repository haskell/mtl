{-# LANGUAGE UndecidableInstances #-}
-- Search for UndecidableInstances to see why this is needed

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.State.Class
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-param classes, functional dependencies)
--
-- MonadState class.
--
--      This module is inspired by the paper
--      /Functional Programming with Overloading and Higher-Order Polymorphism/,
--        Mark P Jones (<http://web.cecs.pdx.edu/~mpj/>)
--          Advanced School of Functional Programming, 1995.

-----------------------------------------------------------------------------

module Control.Monad.State.Class (
    MonadState(..),
    modify,
    gets,
  ) where

import Control.Monad.Trans.Cont
import Control.Monad.Trans.Error
import Control.Monad.Trans.Identity
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS (RWST, get, put)
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS (RWST, get, put)
import qualified Control.Monad.Trans.State.Lazy as Lazy (StateT, get, put)
import qualified Control.Monad.Trans.State.Strict as Strict (StateT, get, put)
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict

import Control.Monad.Trans.Class (lift)
import Control.Monad
import Data.Monoid

-- ---------------------------------------------------------------------------

class (Monad m) => MonadState s m | m -> s where
    -- | Return the state from the internals of the monad.
    get :: m s
    -- | Replace the state inside the monad.
    put :: s -> m ()

-- | Monadic state transformer.
--
--      Maps an old state to a new state inside a state monad.
--      The old state is thrown away.
--
-- >      Main> :t modify ((+1) :: Int -> Int)
-- >      modify (...) :: (MonadState Int a) => a ()
--
--    This says that @modify (+1)@ acts over any
--    Monad that is a member of the @MonadState@ class,
--    with an @Int@ state.

modify :: (MonadState s m) => (s -> s) -> m ()
modify f = do
    s <- get
    put (f s)

-- | Gets specific component of the state, using a projection function
-- supplied.

gets :: (MonadState s m) => (s -> a) -> m a
gets f = do
    s <- get
    return (f s)

instance (Monad m) => MonadState s (Lazy.StateT s m) where
    get = Lazy.get
    put = Lazy.put

instance (Monad m) => MonadState s (Strict.StateT s m) where
    get = Strict.get
    put = Strict.put

instance (Monad m, Monoid w) => MonadState s (LazyRWS.RWST r w s m) where
    get = LazyRWS.get
    put = LazyRWS.put

instance (Monad m, Monoid w) => MonadState s (StrictRWS.RWST r w s m) where
    get = StrictRWS.get
    put = StrictRWS.put

-- ---------------------------------------------------------------------------
-- Instances for other mtl transformers
--
-- All of these instances need UndecidableInstances,
-- because they do not satisfy the coverage condition.

instance (MonadState s m) => MonadState s (ContT r m) where
    get = lift get
    put = lift . put

instance (Error e, MonadState s m) => MonadState s (ErrorT e m) where
    get = lift get
    put = lift . put

instance (MonadState s m) => MonadState s (IdentityT m) where
    get = lift get
    put = lift . put

instance (MonadState s m) => MonadState s (ListT m) where
    get = lift get
    put = lift . put

instance (MonadState s m) => MonadState s (MaybeT m) where
    get = lift get
    put = lift . put

instance (MonadState s m) => MonadState s (ReaderT r m) where
    get = lift get
    put = lift . put

instance (Monoid w, MonadState s m) => MonadState s (Lazy.WriterT w m) where
    get = lift get
    put = lift . put

instance (Monoid w, MonadState s m) => MonadState s (Strict.WriterT w m) where
    get = lift get
    put = lift . put
