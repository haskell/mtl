{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
module Control.Monad.Bypass
  ( Bypass
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Kind (Type)

type Bypass :: ((Type -> Type) -> Type -> Type) -> (Type -> Type) -> Type -> Type
newtype Bypass t m a = Bypass (t m a) deriving (Functor, Applicative, Monad, MonadTrans)

instance MonadReader r m => MonadReader r (Bypass (ReaderT r') m) where
  ask = lift ask
  local f (Bypass (ReaderT x)) = Bypass . ReaderT $ local f . x
  reader = lift . reader

instance (MonadWriter w m, Monoid w') => MonadWriter w (Bypass (WriterT w') m) where
  writer = lift . writer
  tell = lift . tell
  listen (Bypass (WriterT x)) = Bypass $ WriterT $ (\((a, w'), w) -> ((a, w), w')) <$> listen x
  pass (Bypass (WriterT x)) = Bypass $ WriterT $ (\((a,f),w') -> pass $ return ((a,w'),f)) =<< x

instance MonadState s m => MonadState s (Bypass (StateT s') m) where
  get = lift get
  put = lift . put
  state = lift . state

newtype ExampleT m a = ExampleT (ReaderT Int m a) deriving (Functor, Applicative, Monad)

deriving via Bypass (ReaderT Int) m instance MonadReader r m => MonadReader r (ExampleT m)

