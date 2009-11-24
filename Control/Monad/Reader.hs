{-# OPTIONS -fallow-undecidable-instances #-}
{- |
Module      :  Control.Monad.Reader
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

module Control.Monad.Reader (
    module Control.Monad.Reader.Class,
    Reader(..),
    mapReader,
    withReader,
    ReaderT(..),
    mapReaderT,
    withReaderT,
    module Control.Monad,
    module Control.Monad.Fix,
    module Control.Monad.Trans,
    -- * Example 1: Simple Reader Usage
    -- $simpleReaderExample

    -- * Example 2: Modifying Reader Content With @local@
    -- $localExample

    -- * Example 3: @ReaderT@ Monad Transformer
    -- $ReaderTExample
    ) where

import Control.Monad
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import Control.Monad.Fix
import Control.Monad.Instances ()
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Writer.Class

{- |
The parameterizable reader monad.

The @return@ function creates a @Reader@ that ignores the environment,
and produces the given value.

The binding operator @>>=@ produces a @Reader@ that uses the environment
to extract the value its left-hand side,
and then applies the bound function to that value in the same environment.
-}
newtype Reader r a = Reader {
    {- |
    Runs @Reader@ and extracts the final value from it.
    To extract the value apply @(runReader reader)@ to an environment value.  
    Parameters:

    * A @Reader@ to run.

    * An initial environment.
    -}
    runReader :: r -> a
}

mapReader :: (a -> b) -> Reader r a -> Reader r b
mapReader f m = Reader $ f . runReader m

-- | A more general version of 'local'.

withReader :: (r' -> r) -> Reader r a -> Reader r' a
withReader f m = Reader $ runReader m . f

instance Functor (Reader r) where
    fmap f m = Reader $ \r -> f (runReader m r)

instance Monad (Reader r) where
    return a = Reader $ \_ -> a
    m >>= k  = Reader $ \r -> runReader (k (runReader m r)) r

instance MonadFix (Reader r) where
    mfix f = Reader $ \r -> let a = runReader (f a) r in a

instance MonadReader r (Reader r) where
    ask       = Reader id
    local f m = Reader $ runReader m . f

{- |
The reader monad transformer.
Can be used to add environment reading functionality to other monads.
-}
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

mapReaderT :: (m a -> n b) -> ReaderT w m a -> ReaderT w n b
mapReaderT f m = ReaderT $ f . runReaderT m

withReaderT :: (r' -> r) -> ReaderT r m a -> ReaderT r' m a
withReaderT f m = ReaderT $ runReaderT m . f

instance (Monad m) => Functor (ReaderT r m) where
    fmap f m = ReaderT $ \r -> do
        a <- runReaderT m r
        return (f a)

instance (Monad m) => Monad (ReaderT r m) where
    return a = ReaderT $ \_ -> return a
    m >>= k  = ReaderT $ \r -> do
        a <- runReaderT m r
        runReaderT (k a) r
    fail msg = ReaderT $ \_ -> fail msg

instance (MonadPlus m) => MonadPlus (ReaderT r m) where
    mzero       = ReaderT $ \_ -> mzero
    m `mplus` n = ReaderT $ \r -> runReaderT m r `mplus` runReaderT n r

instance (MonadFix m) => MonadFix (ReaderT r m) where
    mfix f = ReaderT $ \r -> mfix $ \a -> runReaderT (f a) r

instance (Monad m) => MonadReader r (ReaderT r m) where
    ask       = ReaderT return
    local f m = ReaderT $ \r -> runReaderT m (f r)

-- ---------------------------------------------------------------------------
-- Instances for other mtl transformers

instance MonadTrans (ReaderT r) where
    lift m = ReaderT $ \_ -> m

instance (MonadIO m) => MonadIO (ReaderT r m) where
    liftIO = lift . liftIO

instance (MonadCont m) => MonadCont (ReaderT r m) where
    callCC f = ReaderT $ \r ->
        callCC $ \c ->
        runReaderT (f (\a -> ReaderT $ \_ -> c a)) r

instance (MonadError e m) => MonadError e (ReaderT r m) where
    throwError       = lift . throwError
    m `catchError` h = ReaderT $ \r -> runReaderT m r
        `catchError` \e -> runReaderT (h e) r

-- Needs -fallow-undecidable-instances
instance (MonadState s m) => MonadState s (ReaderT r m) where
    get = lift get
    put = lift . put

-- This instance needs -fallow-undecidable-instances, because
-- it does not satisfy the coverage condition
instance (MonadWriter w m) => MonadWriter w (ReaderT r m) where
    tell     = lift . tell
    listen m = ReaderT $ \w -> listen (runReaderT m w)
    pass   m = ReaderT $ \w -> pass   (runReaderT m w)

{- $simpleReaderExample

In this example the @Reader@ monad provides access to variable bindings.
Bindings are a 'Map' of integer variables.
The variable @count@ contains number of variables in the bindings.
You can see how to run a Reader monad and retrieve data from it
with 'runReader', how to access the Reader data with 'ask' and 'asks'.

> type Bindings = Map String Int;
>
>-- Returns True if the "count" variable contains correct bindings size.
>isCountCorrect :: Bindings -> Bool
>isCountCorrect bindings = runReader calc_isCountCorrect bindings
>
>-- The Reader monad, which implements this complicated check.
>calc_isCountCorrect :: Reader Bindings Bool
>calc_isCountCorrect = do
>    count <- asks (lookupVar "count")
>    bindings <- ask
>    return (count == (Map.size bindings))
>
>-- The selector function to  use with 'asks'.
>-- Returns value of the variable with specified name.
>lookupVar :: String -> Bindings -> Int
>lookupVar name bindings = fromJust (Map.lookup name bindings)
>
>sampleBindings = Map.fromList [("count",3), ("1",1), ("b",2)]
>
>main = do
>    putStr $ "Count is correct for bindings " ++ (show sampleBindings) ++ ": ";
>    putStrLn $ show (isCountCorrect sampleBindings);
-}

{- $localExample

Shows how to modify Reader content with 'local'.

>calculateContentLen :: Reader String Int
>calculateContentLen = do
>    content <- ask
>    return (length content);
>
>-- Calls calculateContentLen after adding a prefix to the Reader content.
>calculateModifiedContentLen :: Reader String Int
>calculateModifiedContentLen = local ("Prefix " ++) calculateContentLen
>
>main = do
>    let s = "12345";
>    let modifiedLen = runReader calculateModifiedContentLen s
>    let len = runReader calculateContentLen s
>    putStrLn $ "Modified 's' length: " ++ (show modifiedLen)
>    putStrLn $ "Original 's' length: " ++ (show len)
-}

{- $ReaderTExample

Now you are thinking: 'Wow, what a great monad! I wish I could use
Reader functionality in MyFavoriteComplexMonad!'. Don't worry.
This can be easy done with the 'ReaderT' monad transformer.
This example shows how to combine @ReaderT@ with the IO monad.

>-- The Reader/IO combined monad, where Reader stores a string.
>printReaderContent :: ReaderT String IO ()
>printReaderContent = do
>    content <- ask
>    liftIO $ putStrLn ("The Reader Content: " ++ content)
>
>main = do
>    runReaderT printReaderContent "Some Content"
-}
