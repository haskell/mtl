{-# OPTIONS -fallow-undecidable-instances #-}
-- Search for -fallow-undecidable-instances to see why this is needed

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Cont.Class
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-parameter type classes)
--
-- Continuation monad class
--
-----------------------------------------------------------------------------

module Control.Monad.Cont.Class (
    MonadCont(..),
  ) where

class (Monad m) => MonadCont m where
    callCC :: ((a -> m b) -> m a) -> m a

