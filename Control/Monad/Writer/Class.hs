{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
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
    LiftingWriter(..),
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
import Data.Kind (Type)

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


-- | A helper type to decrease boilerplate when defining new transformer
-- instances of 'MonadWriter'.
--
-- @since ????
type LiftingWriter :: ((Type -> Type) -> Type -> Type) -> (Type -> Type) -> Type -> Type
newtype LiftingWriter t m a = LiftingWriter {runLiftingWriter :: t m a}
  deriving (Functor, Applicative, Monad, MonadTrans)


instance (Monoid w', MonadWriter w m) => MonadWriter w (LiftingWriter (LazyRWS.RWST r w' s) m) where
  writer = lift . writer
  tell = lift . tell
  listen (LiftingWriter (LazyRWS.RWST x)) = LiftingWriter $ LazyRWS.RWST $ \r s -> do
    ((a, s, w'), w) <- listen $ x r s
    pure ((a, w), s, w')
  pass (LiftingWriter (LazyRWS.RWST x)) = LiftingWriter $ LazyRWS.RWST $ \r s -> do
    (y, s, w') <- x r s
    a <- pass $ pure y
    pure (a, s, w')

instance (Monoid w', MonadWriter w m) => MonadWriter w (LiftingWriter (StrictRWS.RWST r w' s) m) where
  writer = lift . writer
  tell = lift . tell
  listen (LiftingWriter (StrictRWS.RWST x)) = LiftingWriter $ StrictRWS.RWST $ \r s -> do
    ((a, s, w'), w) <- listen $ x r s
    pure ((a, w), s, w')
  pass (LiftingWriter (StrictRWS.RWST x)) = LiftingWriter $ StrictRWS.RWST $ \r s -> do
    (y, s, w') <- x r s
    a <- pass $ pure y
    pure (a, s, w')

instance (Monoid w', MonadWriter w m) => MonadWriter w (LiftingWriter (CPSRWS.RWST r w' s) m) where
  writer = lift . writer
  tell = lift . tell
  listen (LiftingWriter (CPSRWS.runRWST -> x)) = LiftingWriter $ CPSRWS.rwsT $ \r s -> do
    ((a, s, w'), w) <- listen $ x r s
    pure ((a, w), s, w')
  pass (LiftingWriter (CPSRWS.runRWST -> x)) = LiftingWriter $ CPSRWS.rwsT $ \r s -> do
    (y, s, w') <- x r s
    a <- pass $ pure y
    pure (a, s, w')

instance (Monoid w', MonadWriter w m) => MonadWriter w (LiftingWriter (Lazy.WriterT w') m) where
  writer = lift . writer
  tell = lift . tell
  listen (LiftingWriter (Lazy.WriterT x)) = LiftingWriter $ Lazy.WriterT $ do
    ((a, w'), w) <- listen x
    pure ((a, w), w')
  pass (LiftingWriter (Lazy.WriterT x)) = LiftingWriter $ Lazy.WriterT $ do
    (y, w') <- x
    a <- pass $ pure y
    pure (a, w')

instance (Monoid w', MonadWriter w m) => MonadWriter w (LiftingWriter (Strict.WriterT w') m) where
  writer = lift . writer
  tell = lift . tell
  listen (LiftingWriter (Strict.WriterT x)) = LiftingWriter $ Strict.WriterT $ do
    ((a, w'), w) <- listen x
    pure ((a, w), w')
  pass (LiftingWriter (Strict.WriterT x)) = LiftingWriter $ Strict.WriterT $ do
    (y, w') <- x
    a <- pass $ pure y
    pure (a, w')

instance (Monoid w', MonadWriter w m) => MonadWriter w (LiftingWriter (CPS.WriterT w') m) where
  writer = lift . writer
  tell = lift . tell
  listen (LiftingWriter (CPS.runWriterT -> x)) = LiftingWriter $ CPS.writerT $ do
    ((a, w'), w) <- listen x
    pure ((a, w), w')
  pass (LiftingWriter (CPS.runWriterT -> x)) = LiftingWriter $ CPS.writerT $ do
    (y, w') <- x
    a <- pass $ pure y
    pure (a, w')

