{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
-- Search for UndecidableInstances to see why this is needed

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Writer.Class
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-param classes, functional dependencies)
--
-- The MonadWriter class.
--
--      Inspired by the paper
--      /Functional Programming with Overloading and Higher-Order Polymorphism/,
--        Mark P Jones (<http://web.cecs.pdx.edu/~mpj/pubs/springschool.html>)
--          Advanced School of Functional Programming, 1995.
-----------------------------------------------------------------------------

module Control.Monad.Writer.Class (
    MonadWriter(..),
    listens,
    censor,
    LiftingWriter,
    LiftWriter(..),
    LiftWriterRWS(..),
  ) where

import Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.Except as Except
import Control.Monad.Trans.Identity (IdentityT)
import qualified Control.Monad.Trans.Identity as Identity
import Control.Monad.Trans.Maybe (MaybeT)
import qualified Control.Monad.Trans.Maybe as Maybe
import Control.Monad.Trans.Reader (ReaderT, mapReaderT)
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS 
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS 
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict 
import Control.Monad.Trans.Accum (AccumT)
import qualified Control.Monad.Trans.Accum as Accum
import qualified Control.Monad.Trans.RWS.CPS as CPSRWS
import qualified Control.Monad.Trans.Writer.CPS as CPS
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Kind (Type, Constraint)
import Data.Coerce (coerce)

-- ---------------------------------------------------------------------------
-- MonadWriter class
--
-- tell is like tell on the MUD's it shouts to monad
-- what you want to be heard. The monad carries this 'packet'
-- upwards, merging it if needed (hence the Monoid requirement).
--
-- listen listens to a monad acting, and returns what the monad "said".
--
-- pass lets you provide a writer transformer which changes internals of
-- the written object.

class (Monoid w, Monad m) => MonadWriter w m | m -> w where
    {-# MINIMAL (writer | tell), listen, pass #-}
    -- | @'writer' (a,w)@ embeds a simple writer action.
    writer :: (a,w) -> m a
    writer ~(a, w) = do
      tell w
      return a

    -- | @'tell' w@ is an action that produces the output @w@.
    tell   :: w -> m ()
    tell w = writer ((),w)

    -- | @'listen' m@ is an action that executes the action @m@ and adds
    -- its output to the value of the computation.
    listen :: m a -> m (a, w)
    -- | @'pass' m@ is an action that executes the action @m@, which
    -- returns a value and a function, and returns the value, applying
    -- the function to the output.
    pass   :: m (a, w -> w) -> m a

-- | @'listens' f m@ is an action that executes the action @m@ and adds
-- the result of applying @f@ to the output to the value of the computation.
--
-- * @'listens' f m = 'liftM' (id *** f) ('listen' m)@
listens :: MonadWriter w m => (w -> b) -> m a -> m (a, b)
listens f m = do
    ~(a, w) <- listen m
    return (a, f w)

-- | @'censor' f m@ is an action that executes the action @m@ and
-- applies the function @f@ to its output, leaving the return value
-- unchanged.
--
-- * @'censor' f m = 'pass' ('liftM' (\\x -> (x,f)) m)@
censor :: MonadWriter w m => (w -> w) -> m a -> m a
censor f m = pass $ do
    a <- m
    return (a, f)

-- | @since 2.2.2
instance (Monoid w) => MonadWriter w ((,) w) where
  writer ~(a, w) = (w, a)
  tell w = (w, ())
  listen ~(w, a) = (w, (a, w))
  pass ~(w, (a, f)) = (f w, a)

-- | @since 2.3
instance (Monoid w, Monad m) => MonadWriter w (CPS.WriterT w m) where
    writer = CPS.writer
    tell   = CPS.tell
    listen = CPS.listen
    pass   = CPS.pass

instance (Monoid w, Monad m) => MonadWriter w (Lazy.WriterT w m) where
    writer = Lazy.writer
    tell   = Lazy.tell
    listen = Lazy.listen
    pass   = Lazy.pass

instance (Monoid w, Monad m) => MonadWriter w (Strict.WriterT w m) where
    writer = Strict.writer
    tell   = Strict.tell
    listen = Strict.listen
    pass   = Strict.pass

-- | @since 2.3
instance (Monoid w, Monad m) => MonadWriter w (CPSRWS.RWST r w s m) where
    writer = CPSRWS.writer
    tell   = CPSRWS.tell
    listen = CPSRWS.listen
    pass   = CPSRWS.pass

instance (Monoid w, Monad m) => MonadWriter w (LazyRWS.RWST r w s m) where
    writer = LazyRWS.writer
    tell   = LazyRWS.tell
    listen = LazyRWS.listen
    pass   = LazyRWS.pass

instance (Monoid w, Monad m) => MonadWriter w (StrictRWS.RWST r w s m) where
    writer = StrictRWS.writer
    tell   = StrictRWS.tell
    listen = StrictRWS.listen
    pass   = StrictRWS.pass

-- ---------------------------------------------------------------------------
-- Instances for other mtl transformers
--
-- All of these instances need UndecidableInstances,
-- because they do not satisfy the coverage condition.

-- | @since 2.2
instance MonadWriter w m => MonadWriter w (ExceptT e m) where
    writer = lift . writer
    tell   = lift . tell
    listen = Except.liftListen listen
    pass   = Except.liftPass pass

instance MonadWriter w m => MonadWriter w (IdentityT m) where
    writer = lift . writer
    tell   = lift . tell
    listen = Identity.mapIdentityT listen
    pass   = Identity.mapIdentityT pass

instance MonadWriter w m => MonadWriter w (MaybeT m) where
    writer = lift . writer
    tell   = lift . tell
    listen = Maybe.liftListen listen
    pass   = Maybe.liftPass pass

instance MonadWriter w m => MonadWriter w (ReaderT r m) where
    writer = lift . writer
    tell   = lift . tell
    listen = mapReaderT listen
    pass   = mapReaderT pass

instance MonadWriter w m => MonadWriter w (Lazy.StateT s m) where
    writer = lift . writer
    tell   = lift . tell
    listen = Lazy.liftListen listen
    pass   = Lazy.liftPass pass

instance MonadWriter w m => MonadWriter w (Strict.StateT s m) where
    writer = lift . writer
    tell   = lift . tell
    listen = Strict.liftListen listen
    pass   = Strict.liftPass pass

-- | There are two valid instances for 'AccumT'. It could either:
--
--   1. Lift the operations to the inner @MonadWriter@
--   2. Handle the operations itself, à la a @WriterT@.
--
--   This instance chooses (1), reflecting that the intent
--   of 'AccumT' as a type is different than that of @WriterT@.
--
-- @since 2.3
instance
  ( Monoid w'
  , MonadWriter w m
  ) => MonadWriter w (AccumT w' m) where
    writer = lift . writer
    tell   = lift . tell
    listen = Accum.liftListen listen
    pass   = Accum.liftPass pass

-- | A helper type function to decrease boilerplate when defining new
-- transformer instances of 'MonadWriter'.
--
-- Example of deriving 'MonadWriter' from @m@ and not the 'Lazy.WriterT' transformer.
--
-- @
-- newtype SneakyWriterT m a = SneakyWriterT { runSneakyWriterT :: Lazy.WriterT String m a }
--   deriving (Functor, Applicative, Monad)
--   deriving (MonadWriter w) via LiftingWriter Lazy.WriterT String m
-- @
--
-- Example of deriving 'MonadWriter' from @m@ and not the 'LazyRWS.RWST' transformer.
--
-- @
-- newtype SneakyRWST m a = SneakyRWST { runSneakyRWST :: LazyRWS.RWST () String () m a }
--   deriving (Functor, Applicative, Monad)
--   deriving (MonadWriter w) via LiftingWriter LazyRWS.RWST () String () m
-- @
--
-- | @since ????
type LiftingWriter :: forall t. t
type family LiftingWriter where
  LiftingWriter = LiftWriter
  LiftingWriter = LiftWriterRWS

-- | Do not use directly; use @LiftingWriter@ instead.
--
-- | @since ????
newtype LiftWriter t w (m :: Type -> Type) a = LiftWriter (t w m a)
  deriving (Functor, Applicative, Monad, MonadTrans)

-- | Do not use directly; use @LiftingWriter@ instead.
--
-- | @since ????
newtype LiftWriterRWS t r w s (m :: Type -> Type) a = LiftWriterRWS (t r w s m a)
  deriving (Functor, Applicative, Monad, MonadTrans)

-- | Class that allows new writer transformers to use the existing instance of 'MonadWriter' so that they can be used with 'LiftingWriter' to using the monad's "MonadWriter' instance.
-- By using this class you only have to define 'mapWriterT' instead of 'writer', 'tell', 'listen', and 'pass'.
--
-- | @since ????
type MapWriter :: (Type -> (Type -> Type) -> Type -> Type) -> Constraint
class MapWriter t where mapWriterT :: (Monad m, Monoid w) => (m (a, w) -> m (b, w)) -> t w m a -> t w m b
-- | @since ????
instance MapWriter Lazy.WriterT where mapWriterT = Lazy.mapWriterT
-- | @since ????
instance MapWriter Strict.WriterT where mapWriterT = Strict.mapWriterT
-- | @since ????
instance MapWriter CPS.WriterT where mapWriterT = CPS.mapWriterT

-- | Class that allows new reader writer state transformers to use the existing instance of 'MonadWriter' so that they can be used with 'LiftingWriter' to using the monad's "MonadWriter' instance.
-- By using this class you only have to define 'mapRWST' instead of 'writer', 'tell', 'listen', and 'pass'.
--
-- | @since ????
type MapRWS :: (Type -> Type -> Type -> (Type -> Type) -> Type -> Type) -> Constraint
class MapRWS t where mapRWST :: (Monad m, Monoid w) => (m (a, s, w) -> m (b, s, w)) -> t r w s m a -> t r w s m b
-- | @since ????
instance MapRWS LazyRWS.RWST where mapRWST = LazyRWS.mapRWST
-- | @since ????
instance MapRWS StrictRWS.RWST where mapRWST = StrictRWS.mapRWST
-- | @since ????
instance MapRWS CPSRWS.RWST where mapRWST = CPSRWS.mapRWST

mapLiftWriter :: (t w m a -> t w m b) -> LiftWriter t w m a -> LiftWriter t w m b
mapLiftWriter = coerce

formatWriter :: ((a,b),c) -> ((a,c),b)
formatWriter ((a,b),c) = ((a,c),b)

mapLiftWriterRWS :: (t r w s m a -> t r w s m b) -> LiftWriterRWS t r w s m a -> LiftWriterRWS t r w s m b
mapLiftWriterRWS = coerce

-- | @since ????
instance (MapWriter t, MonadWriter w m, MonadTrans (t w'), Monad (t w' m), Monoid w') => MonadWriter w (LiftWriter t w' m) where
  writer = lift . writer
  tell = lift . tell
  listen = mapLiftWriter $ mapWriterT $ fmap formatWriter . listen
  pass = mapLiftWriter $ mapWriterT $ pass . fmap formatWriter

-- | @since ????
instance (MapRWS t, MonadWriter w m, MonadTrans (t r w' s), Monad (t r w' s m), Monoid w') => MonadWriter w (LiftWriterRWS t r w' s m) where
  writer = lift . writer
  tell = lift . tell
  listen = mapLiftWriterRWS $ mapRWST $ fmap (\((a,b,c),d) -> ((a,d),b,c)) . listen
  pass = mapLiftWriterRWS $ mapRWST $ pass . fmap (\((a,b),c,d) -> ((a,c,d),b))

