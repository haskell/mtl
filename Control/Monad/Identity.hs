{- |
Module      :  Control.Monad.Identity
Copyright   :  (c) Andy Gill 2001,
      (c) Oregon Graduate Institute of Science and Technology 2001,
      (c) Jeff Newbern 2003-2006,
      (c) Andriy Palamarchuk 2006
License     :  BSD-style (see the file libraries/base/LICENSE)

Maintainer  :  libraries@haskell.org
Stability   :  experimental
Portability :  portable

[Computation type:] Simple function application.

[Binding strategy:] The bound function is applied to the input value.
@'Identity' x >>= f == 'Identity' (f x)@

[Useful for:] Monads can be derived from monad transformers applied to the
'Identity' monad.

[Zero and plus:] None.

[Example type:] @'Identity' a@

The @Identity@ monad is a monad that does not embody any computational strategy.
It simply applies the bound function to its input without any modification.
Computationally, there is no reason to use the @Identity@ monad
instead of the much simpler act of simply applying functions to their arguments.
The purpose of the @Identity@ monad is its fundamental role in the theory
of monad transformers.
Any monad transformer applied to the @Identity@ monad yields a non-transformer
version of that monad.

  Inspired by the paper
  /Functional Programming with Overloading and
      Higher-Order Polymorphism/, 
    Mark P Jones (<http://www.cse.ogi.edu/~mpj/>)
	  Advanced School of Functional Programming, 1995.
-}

module Control.Monad.Identity (
	Identity(..),

	module Control.Monad,
	module Control.Monad.Fix,
   ) where

import Control.Monad
import Control.Monad.Fix

{- | Identity wrapper.
Abstraction for wrapping up a object.
If you have an monadic function, say:
  
>   example :: Int -> Identity Int
>   example x = return (x*x)
  
     you can \"run\" it, using

> Main> runIdentity (example 42)
> 1764 :: Int

A typical use of the Identity monad is to derive a monad
from a monad transformer.

@
-- derive the 'Control.Monad.State.State' monad using the 'Control.Monad.State.StateT' monad transformer
type 'Control.Monad.State.State' s a = 'Control.Monad.State.StateT' s 'Identity' a
@

The @'runIdentity'@ label is used in the type definition because it follows
a style of monad definition that explicitly represents monad values as
computations. In this style, a monadic computation is built up using the monadic
operators and then the value of the computation is extracted
using the @run******@ function.
Because the @Identity@ monad does not do any computation, its definition
is trivial.
For a better example of this style of monad,
see the @'Control.Monad.State.State'@ monad.
-}

newtype Identity a = Identity { runIdentity :: a }

-- ---------------------------------------------------------------------------
-- Identity instances for Functor and Monad

instance Functor Identity where
	fmap f m = Identity (f (runIdentity m))

instance Monad Identity where
	return a = Identity a
	m >>= k  = k (runIdentity m)

instance MonadFix Identity where
	mfix f = Identity (fix (runIdentity . f))
