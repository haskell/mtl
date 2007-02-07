{-# OPTIONS -fallow-undecidable-instances #-}
-- Needed for the same reasons as in Reader, State etc

{- |
Module      :  Control.Monad.Error
Copyright   :  (c) Michael Weber <michael.weber@post.rwth-aachen.de> 2001,
               (c) Jeff Newbern 2003-2006,
               (c) Andriy Palamarchuk 2006
License     :  BSD-style (see the file libraries/base/LICENSE)

Maintainer  :  libraries@haskell.org
Stability   :  experimental
Portability :  non-portable (multi-parameter type classes)

[Computation type:] Computations which may fail or throw exceptions.

[Binding strategy:] Failure records information about the cause\/location
of the failure. Failure values bypass the bound function,
other values are used as inputs to the bound function.

[Useful for:] Building computations from sequences of functions that may fail
or using exception handling to structure error handling.

[Zero and plus:] Zero is represented by an empty error and the plus operation
executes its second argument if the first fails.

[Example type:] @'Data.Either' String a@

The Error monad (also called the Exception monad).
-}

{-
  Rendered by Michael Weber <mailto:michael.weber@post.rwth-aachen.de>,
  inspired by the Haskell Monad Template Library from
    Andy Gill (<http://www.cse.ogi.edu/~andy/>)
-}
module Control.Monad.Error (
	Error(..),
	MonadError(..),
	ErrorT(..),
	mapErrorT,
	module Control.Monad,
	module Control.Monad.Fix,
	module Control.Monad.Trans
    -- * Example 1: Custom Error Data Type
    -- $customErrorExample

    -- * Example 2: Using ErrorT Monad Transformer
    -- $ErrorTExample
  ) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad.RWS
import Control.Monad.Cont

import Control.Monad.Instances ()
import System.IO

-- | An exception to be thrown.
-- An instance must redefine at least one of 'noMsg', 'strMsg'.
class Error a where
	-- | Creates an exception without a message.
	-- Default implementation is @'strMsg' \"\"@.
	noMsg  :: a
	-- | Creates an exception with a message.
	-- Default implementation is 'noMsg'.
	strMsg :: String -> a

	noMsg    = strMsg ""
	strMsg _ = noMsg

-- | A string can be thrown as an error.
instance Error String where
	noMsg  = ""
	strMsg = id

instance Error IOError where
	strMsg = userError

{- |
The strategy of combining computations that can throw exceptions
by bypassing bound functions
from the point an exception is thrown to the point that it is handled.

Is parameterized over the type of error information and
the monad type constructor.
It is common to use @'Data.Either' String@ as the monad type constructor
for an error monad in which error descriptions take the form of strings.
In that case and many other common cases the resulting monad is already defined
as an instance of the 'MonadError' class.
You can also define your own error type and\/or use a monad type constructor
other than @'Data.Either' String@ or @'Data.Either' IOError@.
In these cases you will have to explicitly define instances of the 'Error'
and\/or 'MonadError' classes.
-}
class (Monad m) => MonadError e m | m -> e where
	-- | Is used within a monadic computation to begin exception processing.
	throwError :: e -> m a

	{- |
	A handler function to handle previous errors and return to normal execution.
	A common idiom is:

	> do { action1; action2; action3 } `catchError` handler

	where the @action@ functions can call 'throwError'.
	Note that @handler@ and the do-block must have the same return type.
	-}
	catchError :: m a -> (e -> m a) -> m a

instance MonadPlus IO where
	mzero       = ioError (userError "mzero")
	m `mplus` n = m `catch` \_ -> n

instance MonadError IOError IO where
	throwError = ioError
	catchError = catch

-- ---------------------------------------------------------------------------
-- Our parameterizable error monad

instance (Error e) => Monad (Either e) where
	return        = Right
	Left  l >>= _ = Left l
	Right r >>= k = k r
	fail msg      = Left (strMsg msg)

instance (Error e) => MonadPlus (Either e) where
	mzero            = Left noMsg
	Left _ `mplus` n = n
	m      `mplus` _ = m

instance (Error e) => MonadFix (Either e) where
	mfix f = let
		a = f $ case a of
			Right r -> r
			_       -> error "empty mfix argument"
		in a

instance (Error e) => MonadError e (Either e) where
	throwError             = Left
	Left  l `catchError` h = h l
	Right r `catchError` _ = Right r

{- |
The error monad transformer. It can be used to add error handling to other
monads.

The @ErrorT@ Monad structure is parameterized over two things:

 * e - The error type.

 * m - The inner monad.

Here are some examples of use:

> -- wraps IO action that can throw an error e
> type ErrorWithIO e a = ErrorT e IO a
> ==> ErrorT (IO (Either e a))
>
> -- IO monad wrapped in StateT inside of ErrorT
> type ErrorAndStateWithIO e s a = ErrorT e (StateT s IO) a
> ==> ErrorT (StateT s IO (Either e a))
> ==> ErrorT (StateT (s -> IO (Either e a,s)))
-}

newtype ErrorT e m a = ErrorT { runErrorT :: m (Either e a) }

instance (Monad m) => Functor (ErrorT e m) where
	fmap f m = ErrorT $ do
		a <- runErrorT m
		case a of
			Left  l -> return (Left  l)
			Right r -> return (Right (f r))

instance (Monad m, Error e) => Monad (ErrorT e m) where
	return a = ErrorT $ return (Right a)
	m >>= k  = ErrorT $ do
		a <- runErrorT m
		case a of
			Left  l -> return (Left l)
			Right r -> runErrorT (k r)
	fail msg = ErrorT $ return (Left (strMsg msg))

instance (Monad m, Error e) => MonadPlus (ErrorT e m) where
	mzero       = ErrorT $ return (Left noMsg)
	m `mplus` n = ErrorT $ do
		a <- runErrorT m
		case a of
			Left  _ -> runErrorT n
			Right r -> return (Right r)

instance (MonadFix m, Error e) => MonadFix (ErrorT e m) where
	mfix f = ErrorT $ mfix $ \a -> runErrorT $ f $ case a of
		Right r -> r
		_       -> error "empty mfix argument"

instance (Monad m, Error e) => MonadError e (ErrorT e m) where
	throwError l     = ErrorT $ return (Left l)
	m `catchError` h = ErrorT $ do
		a <- runErrorT m
		case a of
			Left  l -> runErrorT (h l)
			Right r -> return (Right r)

instance (Error e) => MonadTrans (ErrorT e) where
	lift m = ErrorT $ do
		a <- m
		return (Right a)

instance (Error e, MonadIO m) => MonadIO (ErrorT e m) where
	liftIO = lift . liftIO

instance (Error e, MonadReader r m) => MonadReader r (ErrorT e m) where
	ask       = lift ask
	local f m = ErrorT $ local f (runErrorT m)

instance (Error e, MonadWriter w m) => MonadWriter w (ErrorT e m) where
	tell     = lift . tell
	listen m = ErrorT $ do
		(a, w) <- listen (runErrorT m)
		return $ case a of
			Left  l -> Left  l
			Right r -> Right (r, w)
	pass   m = ErrorT $ pass $ do
		a <- runErrorT m
		return $ case a of
			Left  l      -> (Left  l, id)
			Right (r, f) -> (Right r, f)

instance (Error e, MonadState s m) => MonadState s (ErrorT e m) where
	get = lift get
	put = lift . put

instance (Error e, MonadCont m) => MonadCont (ErrorT e m) where
	callCC f = ErrorT $
		callCC $ \c ->
		runErrorT (f (\a -> ErrorT $ c (Right a)))

mapErrorT :: (m (Either e a) -> n (Either e' b)) -> ErrorT e m a -> ErrorT e' n b
mapErrorT f m = ErrorT $ f (runErrorT m)

-- ---------------------------------------------------------------------------
-- MonadError instances for other monad transformers

instance (MonadError e m) => MonadError e (ReaderT r m) where
	throwError       = lift . throwError
	m `catchError` h = ReaderT $ \r -> runReaderT m r
		`catchError` \e -> runReaderT (h e) r

instance (Monoid w, MonadError e m) => MonadError e (WriterT w m) where
	throwError       = lift . throwError
	m `catchError` h = WriterT $ runWriterT m
		`catchError` \e -> runWriterT (h e)

instance (MonadError e m) => MonadError e (StateT s m) where
	throwError       = lift . throwError
	m `catchError` h = StateT $ \s -> runStateT m s
		`catchError` \e -> runStateT (h e) s

instance (Monoid w, MonadError e m) => MonadError e (RWST r w s m) where
	throwError       = lift . throwError
	m `catchError` h = RWST $ \r s -> runRWST m r s
		`catchError` \e -> runRWST (h e) r s

{- $customErrorExample
Here is an example that demonstrates the use of a custom 'Error' data type with
the 'ErrorMonad'\'s 'throwError' and 'catchError' exception mechanism.
The example throws an exception if the user enters an empty string
or a string longer than 5 characters. Otherwise it prints length of the string.

>-- This is the type to represent length calculation error.
>data LengthError = EmptyString  -- Entered string was empty.
>          | StringTooLong Int   -- A string is longer than 5 characters.
>                                -- Records a length of the string.
>          | OtherError String   -- Other error, stores the problem description.
>
>-- We make LengthError an instance of the Error class
>-- to be able to throw it as an exception.
>instance Error LengthError where
>  noMsg    = OtherError "A String Error!"
>  strMsg s = OtherError s
>
>-- Converts LengthError to a readable message.
>instance Show LengthError where
>  show EmptyString = "The string was empty!"
>  show (StringTooLong len) =
>      "The length of the string (" ++ (show len) ++ ") is bigger than 5!"
>  show (OtherError msg) = msg
>
>-- For our monad type constructor, we use Either LengthError
>-- which represents failure using Left LengthError
>-- or a successful result of type a using Right a.
>type LengthMonad = Either LengthError
>
>main = do
>  putStrLn "Please enter a string:"
>  s <- getLine
>  reportResult (calculateLength s)
>
>-- Wraps length calculation to catch the errors.
>-- Returns either length of the string or an error.
>calculateLength :: String -> LengthMonad Int
>calculateLength s = (calculateLengthOrFail s) `catchError` Left
>
>-- Attempts to calculate length and throws an error if the provided string is
>-- empty or longer than 5 characters.
>-- The processing is done in Either monad.
>calculateLengthOrFail :: String -> LengthMonad Int
>calculateLengthOrFail [] = throwError EmptyString
>calculateLengthOrFail s | len > 5 = throwError (StringTooLong len)
>                        | otherwise = return len
>  where len = length s
>
>-- Prints result of the string length calculation.
>reportResult :: LengthMonad Int -> IO ()
>reportResult (Right len) = putStrLn ("The length of the string is " ++ (show len))
>reportResult (Left e) = putStrLn ("Length calculation failed with error: " ++ (show e))
-}

{- $ErrorTExample
@'ErrorT'@ monad transformer can be used to add error handling to another monad.
Here is an example how to combine it with an @IO@ monad:

>import Control.Monad.Error
>
>-- An IO monad which can return String failure.
>-- It is convenient to define the monad type of the combined monad,
>-- especially if we combine more monad transformers.
>type LengthMonad = ErrorT String IO
>
>main = do
>  -- runErrorT removes the ErrorT wrapper
>  r <- runErrorT calculateLength
>  reportResult r
>
>-- Asks user for a non-empty string and returns its length.
>-- Throws an error if user enters an empty string.
>calculateLength :: LengthMonad Int
>calculateLength = do
>  -- all the IO operations have to be lifted to the IO monad in the monad stack
>  liftIO $ putStrLn "Please enter a non-empty string: "
>  s <- liftIO getLine
>  if null s
>    then throwError "The string was empty!"
>    else return $ length s
>
>-- Prints result of the string length calculation.
>reportResult :: Either String Int -> IO ()
>reportResult (Right len) = putStrLn ("The length of the string is " ++ (show len))
>reportResult (Left e) = putStrLn ("Length calculation failed with error: " ++ (show e))
-}

