{-# OPTIONS -fallow-undecidable-instances #-}
-- Search for -fallow-undecidable-instances to see why this is needed

{- |
Module      :  Control.Monad.Cont.Class
Copyright   :  (c) The University of Glasgow 2001,
               (c) Jeff Newbern 2003-2007,
               (c) Andriy Palamarchuk 2007
License     :  BSD-style (see the file libraries/base/LICENSE)

Maintainer  :  libraries@haskell.org
Stability   :  experimental
Portability :  non-portable (multi-parameter type classes)

[Computation type:] Computations which can be interrupted and resumed.

[Binding strategy:] Binding a function to a monadic value creates
a new continuation which uses the function as the continuation of the monadic
computation.

[Useful for:] Complex control structures, error handling,
and creating co-routines.

[Zero and plus:] None.

[Example type:] @'Cont' r a@

The Continuation monad represents computations in continuation-passing style
(CPS).
In continuation-passing style function result is not returned,
but instead is passed to another function,
received as a parameter (continuation).
Computations are built up from sequences
of nested continuations, terminated by a final continuation (often @id@)
which produces the final result.
Since continuations are functions which represent the future of a computation,
manipulation of the continuation functions can achieve complex manipulations
of the future of the computation,
such as interrupting a computation in the middle, aborting a portion
of a computation, restarting a computation, and interleaving execution of
computations.
The Continuation monad adapts CPS to the structure of a monad.

Before using the Continuation monad, be sure that you have
a firm understanding of continuation-passing style
and that continuations represent the best solution to your particular
design problem.
Many algorithms which require continuations in other languages do not require
them in Haskell, due to Haskell's lazy semantics.
Abuse of the Continuation monad can produce code that is impossible
to understand and maintain.
-}

module Control.Monad.Cont.Class (
    MonadCont(..),
  ) where

class (Monad m) => MonadCont m where
    {- | @callCC@ (call-with-current-continuation)
    calls a function with the current continuation as its argument.
    Provides an escape continuation mechanism for use with Continuation monads.
    Escape continuations allow to abort the current computation and return
    a value immediately.
    They achieve a similar effect to 'Control.Monad.Error.throwError'
    and 'Control.Monad.Error.catchError'
    within an 'Control.Monad.Error.Error' monad.
    Advantage of this function over calling @return@ is that it makes
    the continuation explicit,
    allowing more flexibility and better control
    (see examples in "Control.Monad.Cont").

    The standard idiom used with @callCC@ is to provide a lambda-expression
    to name the continuation. Then calling the named continuation anywhere
    within its scope will escape from the computation,
    even if it is many layers deep within nested computations.
    -}
    callCC :: ((a -> m b) -> m a) -> m a

