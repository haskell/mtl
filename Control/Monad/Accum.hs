{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Accum
-- Copyright   :  (c) Manuel Bärenz,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-param classes, functional dependencies)
--
-- The MonadAccum class.
--
--      Inspired by the paper
--      /Functional Programming with Overloading and Higher-Order Polymorphism/,
--        Mark P Jones (<http://web.cecs.pdx.edu/~mpj/pubs/springschool.html>)
--          Advanced School of Functional Programming, 1995.
-----------------------------------------------------------------------------

module Control.Monad.Accum (
    module Control.Monad.Accum.Class,
#if MIN_VERSION_transformers(0,5,3)
    module Control.Monad.Trans.Accum,
#endif
  ) where

#if MIN_VERSION_transformers(0,5,3)
import Control.Monad.Trans.Accum (AccumT (..), runAccum, runAccumT, execAccum, execAccumT, evalAccum, evalAccumT, mapAccum, mapAccumT)
#endif
import Control.Monad.Accum.Class
