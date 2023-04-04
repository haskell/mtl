{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Safe #-}
-- Needed because the CPSed versions of Writer and State are secretly State
-- wrappers, which don't force such constraints, even though they should legally
-- be there.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{- |
Module      :  Control.Monad.Error.Class
Copyright   :  (c) Michael Weber <michael.weber@post.rwth-aachen.de> 2001,
               (c) Jeff Newbern 2003-2006,
               (c) Andriy Palamarchuk 2006
               (c) Edward Kmett 2012
License     :  BSD-style (see the file LICENSE)

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

[Example type:] @'Either' 'String' a@

The Error monad (also called the Exception monad).
-}

{-
  Rendered by Michael Weber <mailto:michael.weber@post.rwth-aachen.de>,
  inspired by the Haskell Monad Template Library from
    Andy Gill (<http://web.cecs.pdx.edu/~andy/>)
-}
module Control.Monad.Error.Class (
    MonadError(..),
    liftEither,
    liftMaybe,
    tryError,
    withError,
    handleError,
    mapError,
    modifyError,
  ) where

import Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.Except as ExceptT (catchE, runExceptT, throwE)
import Control.Monad.Trans.Identity (IdentityT)
import qualified Control.Monad.Trans.Identity as Identity
import Control.Monad.Trans.Maybe (MaybeT)
import qualified Control.Monad.Trans.Maybe as Maybe
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS
import qualified Control.Monad.Trans.State.Lazy as LazyState
import qualified Control.Monad.Trans.State.Strict as StrictState
import qualified Control.Monad.Trans.Writer.Lazy as LazyWriter
import qualified Control.Monad.Trans.Writer.Strict as StrictWriter
import Control.Monad.Trans.Accum (AccumT)
import qualified Control.Monad.Trans.Accum as Accum
import qualified Control.Monad.Trans.RWS.CPS as CPSRWS
import qualified Control.Monad.Trans.Writer.CPS as CPSWriter
import Control.Monad.Trans.Class (lift)
import Control.Exception (IOException, catch, ioError)
import Control.Monad (Monad)
import Data.Monoid (Monoid)
import Prelude (Either (Left, Right), Maybe (Nothing), either, maybe, flip, (.), IO, pure, (<$>), (>>=))

{- |
The strategy of combining computations that can throw exceptions
by bypassing bound functions
from the point an exception is thrown to the point that it is handled.

Is parameterized over the type of error information and
the monad type constructor.
It is common to use @'Either' String@ as the monad type constructor
for an error monad in which error descriptions take the form of strings.
In that case and many other common cases the resulting monad is already defined
as an instance of the 'MonadError' class.
You can also define your own error type and\/or use a monad type constructor
other than @'Either' 'String'@ or @'Either' 'IOError'@.
In these cases you will have to explicitly define instances of the 'MonadError'
class.
(If you are using the deprecated "Control.Monad.Error" or
"Control.Monad.Trans.Error", you may also have to define an 'Error' instance.)
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
    {-# MINIMAL throwError, catchError #-}

{- |
Lifts an @'Either' e@ into any @'MonadError' e@.

> do { val <- liftEither =<< action1; action2 }

where @action1@ returns an 'Either' to represent errors.

@since 2.2.2
-}
liftEither :: MonadError e m => Either e a -> m a
liftEither = either throwError pure

{- |
Lifts a @'Maybe'@ into any @'MonadError' e@, using a supplied @e@ as the error if the 'Maybe' is 'Nothing'.

> do { val <- liftMaybe e =<< action1; action2 }

where @action1@ returns a 'Maybe'.
-}
liftMaybe :: MonadError e m => e -> Maybe a -> m a
liftMaybe e = maybe (throwError e) pure

instance MonadError IOException IO where
    throwError = ioError
    catchError = catch

{- | @since 2.2.2 -}
instance MonadError () Maybe where
    throwError ()        = Nothing
    catchError Nothing f = f ()
    catchError x       _ = x

-- ---------------------------------------------------------------------------
-- Our parameterizable error monad

instance MonadError e (Either e) where
    throwError             = Left
    Left  l `catchError` h = h l
    Right r `catchError` _ = Right r

{- | @since 2.2 -}
instance Monad m => MonadError e (ExceptT e m) where
    throwError = ExceptT.throwE
    catchError = ExceptT.catchE

-- ---------------------------------------------------------------------------
-- Instances for other mtl transformers
--
-- All of these instances need UndecidableInstances,
-- because they do not satisfy the coverage condition.

instance MonadError e m => MonadError e (IdentityT m) where
    throwError = lift . throwError
    catchError = Identity.liftCatch catchError

instance MonadError e m => MonadError e (MaybeT m) where
    throwError = lift . throwError
    catchError = Maybe.liftCatch catchError

instance MonadError e m => MonadError e (ReaderT r m) where
    throwError = lift . throwError
    catchError = Reader.liftCatch catchError

instance (Monoid w, MonadError e m) => MonadError e (LazyRWS.RWST r w s m) where
    throwError = lift . throwError
    catchError = LazyRWS.liftCatch catchError

instance (Monoid w, MonadError e m) => MonadError e (StrictRWS.RWST r w s m) where
    throwError = lift . throwError
    catchError = StrictRWS.liftCatch catchError

instance MonadError e m => MonadError e (LazyState.StateT s m) where
    throwError = lift . throwError
    catchError = LazyState.liftCatch catchError

instance MonadError e m => MonadError e (StrictState.StateT s m) where
    throwError = lift . throwError
    catchError = StrictState.liftCatch catchError

instance (Monoid w, MonadError e m) => MonadError e (LazyWriter.WriterT w m) where
    throwError = lift . throwError
    catchError = LazyWriter.liftCatch catchError

instance (Monoid w, MonadError e m) => MonadError e (StrictWriter.WriterT w m) where
    throwError = lift . throwError
    catchError = StrictWriter.liftCatch catchError

-- | @since 2.3
instance (Monoid w, MonadError e m) => MonadError e (CPSRWS.RWST r w s m) where
    throwError = lift . throwError
    catchError = CPSRWS.liftCatch catchError

-- | @since 2.3
instance (Monoid w, MonadError e m) => MonadError e (CPSWriter.WriterT w m) where
    throwError = lift . throwError
    catchError = CPSWriter.liftCatch catchError

-- | @since 2.3
instance
  ( Monoid w
  , MonadError e m
  ) => MonadError e (AccumT w m) where
    throwError = lift . throwError
    catchError = Accum.liftCatch catchError

-- | 'MonadError' analogue to the 'Control.Exception.try' function.
--
-- @since 2.3
tryError :: MonadError e m => m a -> m (Either e a)
tryError action = (Right <$> action) `catchError` (pure . Left)

-- | 'MonadError' analogue to the 'withExceptT' function.
-- Modify the value (but not the type) of an error.  The type is
-- fixed because of the functional dependency @m -> e@.  If you need
-- to change the type of @e@ use 'mapError' or 'modifyError'.
--
-- @since 2.3
withError :: MonadError e m => (e -> e) -> m a -> m a
withError f action = tryError action >>= either (throwError . f) pure

-- | As 'handle' is flipped 'Control.Exception.catch', 'handleError'
-- is flipped 'catchError'.
--
-- @since 2.3
handleError :: MonadError e m => (e -> m a) -> m a -> m a
handleError = flip catchError

-- | 'MonadError' analogue of the 'mapExceptT' function.  The
-- computation is unwrapped, a function is applied to the @Either@, and
-- the result is lifted into the second 'MonadError' instance.
--
-- @since 2.3
mapError :: (MonadError e m, MonadError e' n) => (m (Either e a) -> n (Either e' b)) -> m a -> n b
mapError f action = f (tryError action) >>= liftEither

{- |
A different 'MonadError' analogue to the 'withExceptT' function.
Modify the value (and possibly the type) of an error in an @ExceptT@-transformed
monad, while stripping the @ExceptT@ layer.

This is useful for adapting the 'MonadError' constraint of a computation.

For example:

> data DatabaseError = ...
>
> performDatabaseQuery :: (MonadError DatabaseError m, ...) => m PersistedValue
>
> data AppError
>   = MkDatabaseError DatabaseError
>   | ...
>
> app :: (MonadError AppError m, ...) => m ()

Given these types, @performDatabaseQuery@ cannot be used directly inside
@app@, because the error types don't match. Using 'modifyError', an equivalent
function with a different error type can be constructed:

> performDatabaseQuery' :: (MonadError AppError m, ...) => m PersistedValue
> performDatabaseQuery' = modifyError MkDatabaseError performDatabaseQuery

Since the error types do match, @performDatabaseQuery'@ _can_ be used in @app@,
assuming all other constraints carry over.

This works by instantiating the @m@ in the type of @performDatabaseQuery@ to
@ExceptT DatabaseError m'@, which satisfies the @MonadError DatabaseError@
constraint. Immediately, the @ExceptT DatabaseError@ layer is unwrapped,
producing 'Either' a @DatabaseError@ or a @PersistedValue@. If it's the former,
the error is wrapped in @MkDatabaseError@ and re-thrown in the inner monad,
otherwise the result value is returned.

@since 2.3.1
-}
modifyError :: MonadError e' m => (e -> e') -> ExceptT e m a -> m a
modifyError f m = ExceptT.runExceptT m >>= either (throwError . f) pure
