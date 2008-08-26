-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.State.Class
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-param classes, functional dependencies)
--
-- MonadState class.
--
--      This module is inspired by the paper
--      /Functional Programming with Overloading and
--          Higher-Order Polymorphism/,
--        Mark P Jones (<http://web.cecs.pdx.edu/~mpj/>)
--          Advanced School of Functional Programming, 1995.

-----------------------------------------------------------------------------

module Control.Monad.State.Class (
    -- * MonadState class
    MonadState(..),
    modify,
    gets,
  ) where

-- ---------------------------------------------------------------------------
-- | /get/ returns the state from the internals of the monad.
--
-- /put/ replaces the state inside the monad.

class (Monad m) => MonadState s m | m -> s where
    get :: m s
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

