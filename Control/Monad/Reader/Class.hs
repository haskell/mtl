{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- Search for UndecidableInstances to see why this is needed
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
-- Needed because the CPSed versions of Writer and State are secretly State
-- wrappers, which don't force such constraints, even though they should legally
-- be there.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
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
    LiftingReader(..),
    ) where

import qualified Control.Monad.Trans.Cont as Cont
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Except (ExceptT, mapExceptT)
import Control.Monad.Trans.Identity (IdentityT, mapIdentityT)
import Control.Monad.Trans.Maybe (MaybeT, mapMaybeT)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Reader as ReaderT
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import Control.Monad.Trans.Accum (AccumT)
import qualified Control.Monad.Trans.Accum as Accum
import Control.Monad.Trans.Select (SelectT (SelectT), runSelectT)
import qualified Control.Monad.Trans.RWS.CPS as CPSRWS
import qualified Control.Monad.Trans.Writer.CPS as CPS
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Kind (Type)
import Data.Coerce (coerce)

-- ----------------------------------------------------------------------------
-- class MonadReader
--  asks for the internal (non-mutable) state.

-- | See examples in "Control.Monad.Reader".
-- Note, the partially applied function type @(->) r@ is a simple reader monad.
-- See the @instance@ declaration below.
class Monad m => MonadReader r m | m -> r where
    {-# MINIMAL (ask | reader), local #-}
    -- | Retrieves the monad environment.
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

-- | @since 2.3
instance (Monad m, Monoid w) => MonadReader r (CPSRWS.RWST r w s m) where
    ask = CPSRWS.ask
    local = CPSRWS.local
    reader = CPSRWS.reader

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

-- | @since 2.3
instance (Monoid w, MonadReader r m) => MonadReader r (CPS.WriterT w m) where
    ask   = lift ask
    local = CPS.mapWriterT . local
    reader = lift . reader

instance (Monoid w, MonadReader r m) => MonadReader r (Lazy.WriterT w m) where
    ask   = lift ask
    local = Lazy.mapWriterT . local
    reader = lift . reader

instance (Monoid w, MonadReader r m) => MonadReader r (Strict.WriterT w m) where
    ask   = lift ask
    local = Strict.mapWriterT . local
    reader = lift . reader

-- | @since 2.3
instance
  ( Monoid w
  , MonadReader r m
  ) => MonadReader r (AccumT w m) where
    ask = lift ask
    local = Accum.mapAccumT . local
    reader = lift . reader

-- | @since 2.3
instance
  ( MonadReader r' m
  ) => MonadReader r' (SelectT r m) where
    ask = lift ask
    -- there is no Select.liftLocal
    local f m = SelectT $ \c -> do
      r <- ask
      local f (runSelectT m (local (const r) . c))
    reader = lift . reader

-- | A helper type to decrease boilerplate when defining new transformer
-- instances of 'MonadReader'.
--
-- @
-- newtype SneakyReaderT m a = SneakyReaderT { runSneakyReaderT :: ReaderT String m a }
--   deriving (Functor, Applicative, Monad)
--   deriving (MonadReader r) via LiftingReader (ReaderT String) m
-- @
--
-- @since ????
type LiftingReader :: ((Type -> Type) -> Type -> Type) -> (Type -> Type) -> Type -> Type
newtype LiftingReader t m a = LiftingReader (t m a)
  deriving (Functor, Applicative, Monad, MonadTrans)

mapLiftingReader :: (t m a -> t m b) -> LiftingReader t m a -> LiftingReader t m b
mapLiftingReader = coerce

-- | @since ????
instance (MonadReader r m, Monoid w) => MonadReader r (LiftingReader (LazyRWS.RWST r' w s) m) where
  ask = lift ask
  local = mapLiftingReader . LazyRWS.mapRWST . local
  reader = lift . reader

-- | @since ????
instance (MonadReader r m, Monoid w) => MonadReader r (LiftingReader (StrictRWS.RWST r' w s) m) where
  ask = lift ask
  local = mapLiftingReader . StrictRWS.mapRWST . local
  reader = lift . reader

-- | @since ????
instance (MonadReader r m, Monoid w) => MonadReader r (LiftingReader (CPSRWS.RWST r' w s) m) where
  ask = lift ask
  local = mapLiftingReader . CPSRWS.mapRWST . local
  reader = lift . reader

-- | @since ????
instance MonadReader r m => MonadReader r (LiftingReader (ReaderT r') m) where
  ask = lift ask
  local = mapLiftingReader . ReaderT.mapReaderT . local
  reader = lift . reader

