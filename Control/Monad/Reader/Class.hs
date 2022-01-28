{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
-- Search for UndecidableInstances to see why this is needed
{- |
Module      :  Control.Monad.Reader.Class
Copyright   :  (c) Andy Gill 2001,
               (c) Oregon Graduate Institute of Science and Technology 2001,
               (c) Jeff Newbern 2003-2007,
               (c) Andriy Palamarchuk 2007
License     :  BSD-style (see the file LICENSE)

Maintainer  :  libraries@haskell.org
Stability   :  experimental
Portability :  non-portable (multi-param classes, functional dependencies)

[Computation type:] Computations which read values from a shared environment.

[Binding strategy:] Monad values are functions from the environment to a value.
The bound function is applied to the bound value, and both have access
to the shared environment.

[Useful for:] Maintaining variable bindings, or other shared environment.

[Zero and plus:] None.

[Example type:] @'Reader' [(String,Value)] a@

The 'Reader' monad (also called the Environment monad).
Represents a computation, which can read values from
a shared environment, pass values from function to function,
and execute sub-computations in a modified environment.
Using 'Reader' monad for such computations is often clearer and easier
than using the 'Control.Monad.State.State' monad.

  Inspired by the paper
  /Functional Programming with Overloading and Higher-Order Polymorphism/,
    Mark P Jones (<http://web.cecs.pdx.edu/~mpj/>)
    Advanced School of Functional Programming, 1995.
-}

module Control.Monad.Reader.Class (
    MonadReader(..),
    asks,
    ) where

import Control.Monad.Trans.Cont as Cont
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Reader as ReaderT (ask, local, reader)
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS (RWST, ask, local, reader)
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS (RWST, ask, local, reader)
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict

#if MIN_VERSION_transformers(0,5,3)
import Control.Monad.Trans.Accum as Accum
import Control.Monad.Trans.Select as Select
#endif

#if MIN_VERSION_transformers(0,5,6)
import qualified Control.Monad.Trans.RWS.CPS as CPSRWS (RWST, ask, local, reader)
import Control.Monad.Trans.Writer.CPS as CPS
#endif

import Control.Monad.Trans.Class (lift)
import Control.Monad
import Data.Monoid

-- ----------------------------------------------------------------------------
-- class MonadReader
--  asks for the internal (non-mutable) state.

-- | Monads with a notion of readable environment.
--
-- See examples in "Control.Monad.Reader".
-- Note, the partially applied function type @(->) r@ is a simple reader monad.
-- See the @instance@ declaration below.
--
-- === Laws
--
-- 'ask' has no side effects, and produces the same result at any time.
--
-- @
-- 'ask' '>>' m    =   m
-- 'ask' '>>=' \\s1 -> 'ask' '>>=' \\s2 -> k s1 s2   =   'ask' '>>=' \\s -> k s s
--
-- m '<*>' 'ask'   =   'ask' 'Control.Applicative.<**>' m
-- @
--
-- @'local' f@ applies @f@ to the environment produced by 'ask'.
--
-- @
-- 'local' f 'ask'   =   f '<$>' 'ask'
-- 'local' f u     =   'ask' '>>=' \\s -> 'local' (\\_ -> f s) u
-- @
--
-- 'local' is a monoid morphism from @(r -> r)@ to (reversed) @(m a -> m a)@
-- (i.e., @('Data.Monoid.Endo' r -> 'Data.Monoid.Dual' ('Data.Monoid.Endo' (m a)))@).
--
-- @
-- 'local' 'id'          = 'id'
-- 'local' g '.' 'local' f = 'local' (f '.' g)
-- @
--
-- 'local' is a monad morphism from 'm' to 'm'.
--
-- @
-- 'local' f ('pure' x)   =  'pure' x
-- 'local' f (a '>>=' k)  =  'local' f a '>>=' \\x -> 'local' f (k x)
-- @
--
-- 'reader' must be equivalent to its default definition in terms of 'ask',
-- and conversely.
--
-- Under that last condition, a property which is equivalent to the first two
-- laws is that 'reader' must be a monad morphism from @'Reader' r@ to 'm'.
--
-- Another property equivalent to the first three laws is that there
-- is a monad morphism @phi :: forall a. 'ReaderT' r m a -> m a@ such that
-- @phi 'ask' = 'ask'@ and @phi . 'lift' = 'id'@.
class Monad m => MonadReader r m | m -> r where
#if __GLASGOW_HASKELL__ >= 707
    {-# MINIMAL (ask | reader), local #-}
#endif
    -- | Retrieves the environment.
    ask   :: m r
    ask = reader id

    -- | Executes a computation in a modified environment.
    local :: (r -> r) -- ^ The function to modify the environment.
          -> m a      -- ^ @Reader@ to run in the modified environment.
          -> m a

    -- | Retrieves a function of the current environment.
    reader :: (r -> a) -- ^ The selector function to apply to the environment.
           -> m a
    reader f = do
      r <- ask
      return (f r)

-- | Retrieves a function of the current environment.
asks :: MonadReader r m
    => (r -> a) -- ^ The selector function to apply to the environment.
    -> m a
asks = reader

-- ----------------------------------------------------------------------------
-- The partially applied function type is a simple reader monad

instance MonadReader r ((->) r) where
    ask       = id
    local f m = m . f
    reader    = id

instance Monad m => MonadReader r (ReaderT r m) where
    ask = ReaderT.ask
    local = ReaderT.local
    reader = ReaderT.reader

#if MIN_VERSION_transformers(0,5,6)
-- | @since 2.3
instance (Monad m, Monoid w) => MonadReader r (CPSRWS.RWST r w s m) where
    ask = CPSRWS.ask
    local = CPSRWS.local
    reader = CPSRWS.reader
#endif

instance (Monad m, Monoid w) => MonadReader r (LazyRWS.RWST r w s m) where
    ask = LazyRWS.ask
    local = LazyRWS.local
    reader = LazyRWS.reader

instance (Monad m, Monoid w) => MonadReader r (StrictRWS.RWST r w s m) where
    ask = StrictRWS.ask
    local = StrictRWS.local
    reader = StrictRWS.reader

-- ---------------------------------------------------------------------------
-- Instances for other mtl transformers
--
-- All of these instances need UndecidableInstances,
-- because they do not satisfy the coverage condition.

instance MonadReader r' m => MonadReader r' (ContT r m) where
    ask   = lift ask
    local = Cont.liftLocal ask local
    reader = lift . reader

{- | @since 2.2 -}
instance MonadReader r m => MonadReader r (ExceptT e m) where
    ask   = lift ask
    local = mapExceptT . local
    reader = lift . reader

instance MonadReader r m => MonadReader r (IdentityT m) where
    ask   = lift ask
    local = mapIdentityT . local
    reader = lift . reader

instance MonadReader r m => MonadReader r (MaybeT m) where
    ask   = lift ask
    local = mapMaybeT . local
    reader = lift . reader

instance MonadReader r m => MonadReader r (Lazy.StateT s m) where
    ask   = lift ask
    local = Lazy.mapStateT . local
    reader = lift . reader

instance MonadReader r m => MonadReader r (Strict.StateT s m) where
    ask   = lift ask
    local = Strict.mapStateT . local
    reader = lift . reader

#if MIN_VERSION_transformers(0,5,6)
-- | @since 2.3
instance (Monoid w, MonadReader r m) => MonadReader r (CPS.WriterT w m) where
    ask   = lift ask
    local = CPS.mapWriterT . local
    reader = lift . reader
#endif

instance (Monoid w, MonadReader r m) => MonadReader r (Lazy.WriterT w m) where
    ask   = lift ask
    local = Lazy.mapWriterT . local
    reader = lift . reader

instance (Monoid w, MonadReader r m) => MonadReader r (Strict.WriterT w m) where
    ask   = lift ask
    local = Strict.mapWriterT . local
    reader = lift . reader

#if MIN_VERSION_transformers(0,5,3)
-- | @since 2.3
instance
  ( Monoid w
  , MonadReader r m
#if !MIN_VERSION_base(4,8,0)
  , Functor m
#endif
  ) => MonadReader r (AccumT w m) where
    ask = lift ask
    local = Accum.mapAccumT . local
    reader = lift . reader

-- | @since 2.3
instance
  ( MonadReader r' m
#if !MIN_VERSION_base(4,8,0)
  , Functor m
#endif
  ) => MonadReader r' (SelectT r m) where
    ask = lift ask
    -- there is no Select.liftLocal
    local f m = SelectT $ \c -> do
      r <- ask
      local f (runSelectT m (local (const r) . c))
    reader = lift . reader
#endif
