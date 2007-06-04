{-# OPTIONS -fallow-undecidable-instances #-}
-- Search for -fallow-undecidable-instances to see why this is needed

{- |
Module      :  Control.Monad.Cont
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

module Control.Monad.Cont (
    module Control.Monad.Cont.Class,
    Cont(..),
    mapCont,
    withCont,
    ContT(..),
    mapContT,
    withContT,
    module Control.Monad,
    module Control.Monad.Trans,
    -- * Example 1: Simple Continuation Usage
    -- $simpleContExample

    -- * Example 2: Using @callCC@
    -- $callCCExample
    
    -- * Example 3: Using @ContT@ Monad Transformer
    -- $ContTExample
  ) where

import Control.Monad
import Control.Monad.Cont.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans

{- |
Continuation monad.
@Cont r a@ is a CPS computation that produces an intermediate result
of type @a@ within a CPS computation whose final result type is @r@.

The @return@ function simply creates a continuation which passes the value on.

The @>>=@ operator adds the bound function into the continuation chain.
-}
newtype Cont r a = Cont {

    {- | Runs a CPS computation, returns its result after applying
    the final continuation to it.
    Parameters:

    * a continuation computation (@Cont@).

    * the final continuation, which produces the final result (often @id@).
    -}
    runCont :: (a -> r) -> r
}

mapCont :: (r -> r) -> Cont r a -> Cont r a
mapCont f m = Cont $ f . runCont m

withCont :: ((b -> r) -> (a -> r)) -> Cont r a -> Cont r b
withCont f m = Cont $ runCont m . f

instance Functor (Cont r) where
    fmap f m = Cont $ \c -> runCont m (c . f)

instance Monad (Cont r) where
    return a = Cont ($ a)
    m >>= k  = Cont $ \c -> runCont m $ \a -> runCont (k a) c

instance MonadCont (Cont r) where
    callCC f = Cont $ \c -> runCont (f (\a -> Cont $ \_ -> c a)) c

{- |
The continuation monad transformer.
Can be used to add continuation handling to other monads.
-}
newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }

mapContT :: (m r -> m r) -> ContT r m a -> ContT r m a
mapContT f m = ContT $ f . runContT m

withContT :: ((b -> m r) -> (a -> m r)) -> ContT r m a -> ContT r m b
withContT f m = ContT $ runContT m . f

instance (Monad m) => Functor (ContT r m) where
    fmap f m = ContT $ \c -> runContT m (c . f)

instance (Monad m) => Monad (ContT r m) where
    return a = ContT ($ a)
    m >>= k  = ContT $ \c -> runContT m (\a -> runContT (k a) c)

instance (Monad m) => MonadCont (ContT r m) where
    callCC f = ContT $ \c -> runContT (f (\a -> ContT $ \_ -> c a)) c

-- ---------------------------------------------------------------------------
-- Instances for other mtl transformers

instance MonadTrans (ContT r) where
    lift m = ContT (m >>=)

instance (MonadIO m) => MonadIO (ContT r m) where
    liftIO = lift . liftIO

-- Needs -fallow-undecidable-instances
instance (MonadReader r' m) => MonadReader r' (ContT r m) where
    ask       = lift ask
    local f m = ContT $ \c -> do
        r <- ask
        local f (runContT m (local (const r) . c))

-- Needs -fallow-undecidable-instances
instance (MonadState s m) => MonadState s (ContT r m) where
    get = lift get
    put = lift . put

{- $simpleContExample
Calculating length of a list continuation-style:

>calculateLength :: [a] -> Cont r Int
>calculateLength l = return (length l)

Here we use @calculateLength@ by making it to pass its result to @print@:

>main = do
>  runCont (calculateLength "123") print
>  -- result: 3

It is possible to chain 'Cont' blocks with @>>=@.

>double :: Int -> Cont r Int
>double n = return (n * 2)
>
>main = do
>  runCont (calculateLength "123" >>= double) print
>  -- result: 6
-}

{- $callCCExample
This example gives a taste of how escape continuations work, shows a typical
pattern for their usage.

>-- Returns a string depending on the length of the name parameter.
>-- If the provided string is empty, returns an error.
>-- Otherwise, returns a welcome message.
>whatsYourName :: String -> String
>whatsYourName name =
>  (`runCont` id) $ do                      -- 1
>    response <- callCC $ \exit -> do       -- 2
>      validateName name exit               -- 3
>      return $ "Welcome, " ++ name ++ "!"  -- 4
>    return response                        -- 5
>
>validateName name exit = do
>  when (null name) (exit "You forgot to tell me your name!")

Here is what this example does:

(1) Runs an anonymous 'Cont' block and extracts value from it with
@(\`runCont\` id)@. Here @id@ is the continuation, passed to the @Cont@ block.

(1) Binds @response@ to the result of the following 'callCC' block,
binds @exit@ to the continuation.

(1) Validates @name@.
This approach illustrates advantage of using 'callCC' over @return@.
We pass the continuation to @validateName@,
and interrupt execution of the @Cont@ block from /inside/ of @validateName@.

(1) Returns the welcome message from the @callCC@ block.
This line is not executed if @validateName@ fails.

(1) Returns from the @Cont@ block.
-}

{-$ContTExample
'ContT' can be used to add continuation handling to other monads.
Here is an example how to combine it with @IO@ monad:

>import Control.Monad.Cont
>import System.IO
>
>main = do
>  hSetBuffering stdout NoBuffering
>  runContT (callCC askString) reportResult
>
>askString :: (String -> ContT () IO String) -> ContT () IO String
>askString next = do
>  liftIO $ putStrLn "Please enter a string"
>  s <- liftIO $ getLine
>  next s
>
>reportResult :: String -> IO ()
>reportResult s = do
>  putStrLn ("You entered: " ++ s)

Action @askString@ requests user to enter a string,
and passes it to the continuation.
@askString@ takes as a parameter a continuation taking a string parameter,
and returning @IO ()@.
Compare its signature to 'runContT' definition.
-}
