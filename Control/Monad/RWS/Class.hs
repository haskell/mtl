{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- Search for UndecidableInstances to see why this is needed
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Safe #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.RWS.Class
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-param classes, functional dependencies)
--
-- Declaration of the MonadRWS class.
--
--      Inspired by the paper
--      /Functional Programming with Overloading and Higher-Order Polymorphism/,
--        Mark P Jones (<http://web.cecs.pdx.edu/~mpj/>)
--          Advanced School of Functional Programming, 1995.
-----------------------------------------------------------------------------

module Control.Monad.RWS.Class (
    MonadRWS,
    module Control.Monad.Reader.Class,
    module Control.Monad.State.Class,
    module Control.Monad.Writer.Class,
  ) where

import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Writer.Class

import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Identity (IdentityT)
import qualified Control.Monad.Trans.RWS.CPS as CPS (RWST)
import qualified Control.Monad.Trans.RWS.Lazy as Lazy (RWST)
import qualified Control.Monad.Trans.RWS.Strict as Strict (RWST)

class (Monoid w, MonadReader r m, MonadWriter w m, MonadState s m)
   => MonadRWS r w s m | m -> r, m -> w, m -> s

-- | @since 2.3
instance (Monoid w, Monad m) => MonadRWS r w s (CPS.RWST r w s m)

instance (Monoid w, Monad m) => MonadRWS r w s (Lazy.RWST r w s m)

instance (Monoid w, Monad m) => MonadRWS r w s (Strict.RWST r w s m)

---------------------------------------------------------------------------
-- Instances for other mtl transformers
--
-- All of these instances need UndecidableInstances,
-- because they do not satisfy the coverage condition.

-- | @since 2.2
instance MonadRWS r w s m => MonadRWS r w s (ExceptT e m)
instance MonadRWS r w s m => MonadRWS r w s (IdentityT m)
instance MonadRWS r w s m => MonadRWS r w s (MaybeT m)
