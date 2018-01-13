{-# LANGUAGE CPP #-}
{- |
Module      :  Control.Monad.Accum
Copyright   :  ???
License     :  BSD-style (see the file LICENSE)

Maintainer  :  libraries@haskell.org
Stability   :  experimental
Portability :  non-portable (multi-parameter type classes)

Provides append-only accumulation during the computation.  For more general
access, use "Control.Monad.State" instead.
-}

{-
  Rendered by Michael Weber <mailto:michael.weber@post.rwth-aachen.de>,
  inspired by the Haskell Monad Template Library from
    Andy Gill (<http://web.cecs.pdx.edu/~andy/>)
-}
module Control.Monad.Accum
  (
    -- * MonadAccum class
    MonadAccum(..),
    looks,
    -- * The Accum monad
    Accum,
    runAccum,
    evalAccum,
    execAccum,
    mapAccum,
    -- * The AccumT monad transformer
    AccumT(AccumT),
    runAccumT,
    evalAccumT,
    execAccumT,
    mapAccumT,
    module Control.Monad,
    module Control.Monad.Fix,
    module Control.Monad.Trans,
  ) where

import Control.Monad.Accum.Class
import Control.Monad.Trans
import Control.Monad.Trans.Accum
        (Accum, runAccum, evalAccum, execAccum, mapAccum,
         AccumT(AccumT), runAccumT, evalAccumT, execAccumT, mapAccumT)

import Control.Monad
import Control.Monad.Fix

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 707
import Control.Monad.Instances ()
#endif

