{-# LANGUAGE Safe #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- Search for UndecidableInstances to see why this is needed
{-# LANGUAGE UndecidableInstances #-}
-- Needed because the CPSed versions of Writer and State are secretly State
-- wrappers, which don't force such constraints, even though they should legally
-- be there.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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
    modify',
    gets
  ) where

import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.Maybe (MaybeT) 
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import Control.Monad.Trans.Accum (AccumT)
import Control.Monad.Trans.Select (SelectT)
import qualified Control.Monad.Trans.RWS.CPS as CPSRWS
import qualified Control.Monad.Trans.Writer.CPS as CPS
import Control.Monad.Trans.Class (lift)

-- ---------------------------------------------------------------------------

-- | Minimal definition is either both of @get@ and @put@ or just @state@
class Monad m => MonadState s m | m -> s where
    -- | Return the state from the internals of the monad.
    get :: m s
    get = state (\s -> (s, s))

    -- | Replace the state inside the monad.
    put :: s -> m ()
    put s = state (\_ -> ((), s))

    -- | Embed a simple state action into the monad.
    state :: (s -> (a, s)) -> m a
    state f = do
      s <- get
      let ~(a, s') = f s
      put s'
      return a
    {-# MINIMAL state | get, put #-}

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
modify :: MonadState s m => (s -> s) -> m ()
modify f = state (\s -> ((), f s))

-- | A variant of 'modify' in which the computation is strict in the
-- new state.
--
-- @since 2.2
modify' :: MonadState s m => (s -> s) -> m ()
modify' f = do
  s' <- get
  put $! f s'

-- | Gets specific component of the state, using a projection function
-- supplied.
gets :: MonadState s m => (s -> a) -> m a
gets f = do
    s <- get
    return (f s)

instance Monad m => MonadState s (Lazy.StateT s m) where
    get = Lazy.get
    put = Lazy.put
    state = Lazy.state

instance Monad m => MonadState s (Strict.StateT s m) where
    get = Strict.get
    put = Strict.put
    state = Strict.state

-- | @since 2.3
instance (Monad m, Monoid w) => MonadState s (CPSRWS.RWST r w s m) where
    get = CPSRWS.get
    put = CPSRWS.put
    state = CPSRWS.state

instance (Monad m, Monoid w) => MonadState s (LazyRWS.RWST r w s m) where
    get = LazyRWS.get
    put = LazyRWS.put
    state = LazyRWS.state

instance (Monad m, Monoid w) => MonadState s (StrictRWS.RWST r w s m) where
    get = StrictRWS.get
    put = StrictRWS.put
    state = StrictRWS.state

-- ---------------------------------------------------------------------------
-- Instances for other mtl transformers
--
-- All of these instances need UndecidableInstances,
-- because they do not satisfy the coverage condition.

instance MonadState s m => MonadState s (ContT r m) where
    get = lift get
    put = lift . put
    state = lift . state

-- | @since 2.2
instance MonadState s m => MonadState s (ExceptT e m) where
    get = lift get
    put = lift . put
    state = lift . state

instance MonadState s m => MonadState s (IdentityT m) where
    get = lift get
    put = lift . put
    state = lift . state

instance MonadState s m => MonadState s (MaybeT m) where
    get = lift get
    put = lift . put
    state = lift . state

instance MonadState s m => MonadState s (ReaderT r m) where
    get = lift get
    put = lift . put
    state = lift . state

-- | @since 2.3
instance (Monoid w, MonadState s m) => MonadState s (CPS.WriterT w m) where
    get = lift get
    put = lift . put
    state = lift . state

instance (Monoid w, MonadState s m) => MonadState s (Lazy.WriterT w m) where
    get = lift get
    put = lift . put
    state = lift . state

instance (Monoid w, MonadState s m) => MonadState s (Strict.WriterT w m) where
    get = lift get
    put = lift . put
    state = lift . state

-- | @since 2.3
instance
  ( Monoid w
  , MonadState s m
  ) => MonadState s (AccumT w m) where
    get = lift get
    put = lift . put
    state = lift . state

-- | @since 2.3
instance MonadState s m => MonadState s (SelectT r m) where
    get = lift get
    put = lift . put
    state = lift . state
