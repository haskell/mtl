{-# LANGUAGE Safe #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Writer.Strict
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-param classes, functional dependencies)
--
-- Strict writer monads that use continuation-passing-style to achieve constant
-- space usage.
--
--      Inspired by the paper
--      /Functional Programming with Overloading and Higher-Order Polymorphism/,
--        Mark P Jones (<http://web.cecs.pdx.edu/~mpj/pubs/springschool.html>)
--          Advanced School of Functional Programming, 1995.
--
-- /Since: mtl-2.3, transformers-0.5.6/
-----------------------------------------------------------------------------

module Control.Monad.Writer.CPS (
    -- * MonadWriter class
    MonadWriter.MonadWriter(..),
    MonadWriter.listens,
    MonadWriter.censor,
    -- * The Writer monad
    Writer,
    runWriter,
    execWriter,
    mapWriter,
    -- * The WriterT monad transformer
    WriterT,
    execWriterT,
    mapWriterT,
    module Control.Monad.Trans,
  ) where

import qualified Control.Monad.Writer.Class as MonadWriter
import Control.Monad.Trans
import Control.Monad.Trans.Writer.CPS (
        Writer, runWriter, execWriter, mapWriter,
        WriterT, execWriterT, mapWriterT)
