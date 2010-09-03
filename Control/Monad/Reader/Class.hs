{-# LANGUAGE UndecidableInstances #-}
{- |
Module      :  Control.Monad.Reader.Class
Copyright   :  (c) Andy Gill 2001,
               (c) Oregon Graduate Institute of Science and Technology 2001,
               (c) Jeff Newbern 2003-2007,
               (c) Andriy Palamarchuk 2007
License     :  BSD-style (see the file libraries/base/LICENSE)

Maintainer  :  libraries@haskell.org
Stability   :  experimental
Portability :  non-portable (multi-param classes, functional dependencies)

[Computation type:] Computations which read values from a shared environment.

[Binding strategy:] Monad values are functions from the environment to a value.
The bound function is applied to the bound value, and both have access
to the shared environment.

[Useful for:] Maintaining variable bindings, or other shared environment.

[Zero and plus:] None.

[Example type:] @'Reader' [(String,Value)] a@

The 'Reader' monad (also called the Environment monad).
Represents a computation, which can read values from
a shared environment, pass values from function to function,
and execute sub-computations in a modified environment.
Using 'Reader' monad for such computations is often clearer and easier
than using the 'Control.Monad.State.State' monad.

  Inspired by the paper
  /Functional Programming with Overloading and
      Higher-Order Polymorphism/, 
    Mark P Jones (<http://web.cecs.pdx.edu/~mpj/>)
    Advanced School of Functional Programming, 1995.
-}

module Control.Monad.Reader.Class (
    MonadReader(..),
    asks,
    ) where

import Control.Monad.Instances ()

{- |
See examples in "Control.Monad.Reader".
Note, the partially applied function type @(->) r@ is a simple reader monad.
See the @instance@ declaration below.
-}
class (Monad m) => MonadReader r m | m -> r where
    -- | Retrieves the monad environment.
    ask   :: m r
    {- | Executes a computation in a modified environment. Parameters:

    * The function to modify the environment.

    * @Reader@ to run.

    * The resulting @Reader@.
    -}
    local :: (r -> r) -> m a -> m a

-- ----------------------------------------------------------------------------
-- The partially applied function type is a simple reader monad

instance MonadReader r ((->) r) where
    ask       = id
    local f m = m . f

{- |
Retrieves a function of the current environment. Parameters:

* The selector function to apply to the environment.

See an example in "Control.Monad.Reader".
-}
asks :: (MonadReader r m) => (r -> a) -> m a
asks f = do
    r <- ask
    return (f r)

