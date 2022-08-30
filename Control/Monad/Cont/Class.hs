{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
-- Needed because the CPSed versions of Writer and State are secretly State
-- wrappers, which don't force such constraints, even though they should legally
-- be there.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{- |
Module      :  Control.Monad.Cont.Class
Copyright   :  (c) The University of Glasgow 2001,
               (c) Jeff Newbern 2003-2007,
               (c) Andriy Palamarchuk 2007
License     :  BSD-style (see the file LICENSE)

Maintainer  :  libraries@haskell.org
Stability   :  experimental
Portability :  portable

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
    label,
    label_,
    liftCallCC,
  ) where

import Data.Kind (Type)
import Control.Monad.Fix (fix)
import Control.Monad.Trans.Cont (ContT)
import qualified Control.Monad.Trans.Cont as ContT
import Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.Except as Except
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
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Signatures (CallCC)
import Control.Monad (join)

class Monad m => MonadCont (m :: Type -> Type) where
    {- | @callCC@ (call-with-current-continuation)
    calls a function with the current continuation as its argument.
    Provides an escape continuation mechanism for use with Continuation monads.
    Escape continuations allow to abort the current computation and return
    a value immediately.
    They achieve a similar effect to 'Control.Monad.Error.Class.throwError'
    and 'Control.Monad.Error.Class.catchError'
    within an 'Control.Monad.Except.Except' monad.
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
    {-# MINIMAL callCC #-}

-- | @since 2.3.1
instance forall k (r :: k) (m :: (k -> Type)) . MonadCont (ContT r m) where
    callCC = ContT.callCC

-- ---------------------------------------------------------------------------
-- Instances for other mtl transformers

-- | @since 2.2
instance MonadCont m => MonadCont (ExceptT e m) where
    callCC = Except.liftCallCC callCC

instance MonadCont m => MonadCont (IdentityT m) where
    callCC = Identity.liftCallCC callCC

instance MonadCont m => MonadCont (MaybeT m) where
    callCC = Maybe.liftCallCC callCC

instance MonadCont m => MonadCont (ReaderT r m) where
    callCC = Reader.liftCallCC callCC

instance (Monoid w, MonadCont m) => MonadCont (LazyRWS.RWST r w s m) where
    callCC = LazyRWS.liftCallCC' callCC

instance (Monoid w, MonadCont m) => MonadCont (StrictRWS.RWST r w s m) where
    callCC = StrictRWS.liftCallCC' callCC

instance MonadCont m => MonadCont (LazyState.StateT s m) where
    callCC = LazyState.liftCallCC' callCC

instance MonadCont m => MonadCont (StrictState.StateT s m) where
    callCC = StrictState.liftCallCC' callCC

instance (Monoid w, MonadCont m) => MonadCont (LazyWriter.WriterT w m) where
    callCC = LazyWriter.liftCallCC callCC

instance (Monoid w, MonadCont m) => MonadCont (StrictWriter.WriterT w m) where
    callCC = StrictWriter.liftCallCC callCC

-- | @since 2.3
instance (Monoid w, MonadCont m) => MonadCont (CPSRWS.RWST r w s m) where
    callCC = CPSRWS.liftCallCC' callCC

-- | @since 2.3
instance (Monoid w, MonadCont m) => MonadCont (CPSWriter.WriterT w m) where
    callCC = CPSWriter.liftCallCC callCC

-- | @since 2.3
instance
  ( Monoid w
  , MonadCont m
  ) => MonadCont (AccumT w m) where
    callCC = Accum.liftCallCC callCC

-- | Introduces a recursive binding to the continuation.
-- Due to the use of @callCC@, calling the continuation will interrupt execution
-- of the current block creating an effect similar to goto/setjmp in C.
--
-- @since 2.3.1
--
label :: MonadCont m  => a -> m (a -> m b, a)
label a = callCC $ \k -> let go b = k (go, b) in return (go, a)

-- | Simplified version of `label` without arguments.
-- 
-- @since 2.3.1
--
label_ :: MonadCont m => m (m a)
label_ = callCC $ return . fix

-- | Lift a 'ContT.callCC'-style function through any 'MonadTrans'. 
--
-- = Note
--
-- For any function @f@, @'liftCallCC f'@ satisfies the [uniformity
-- condition](https://hackage.haskell.org/package/transformers-0.5.6.2/docs/Control-Monad-Signatures.html#t:CallCC)
-- provided that @f@ is quasi-algebraic. More specifically, for any @g@, we must have:
--
-- > 'join' '$' f (\exit -> 'pure' '$' g (exit '.' 'pure') = f g
--
-- 'ContT.callCC' is quasi-algebraic; furthermore, for any quasi-algebraic @f@,
-- @'liftCallCC' f@ is also quasi-algebraic. 
--
-- = See also
--
-- * [Proof of quasi-algebraic
-- properties](https://gist.github.com/KingoftheHomeless/5927257cc7f6f8a2da685a2045dac204)
-- * [Original issue](https://github.com/haskell/mtl/issues/77)
--
-- @since 2.3.1
liftCallCC :: 
  forall (t :: (Type -> Type) -> Type -> Type) (m :: Type -> Type) (a :: Type) (b :: Type) . 
  (MonadTrans t, Monad m, forall (m' :: Type -> Type) . Monad m' => Monad (t m')) => 
  CallCC m (t m a) b -> CallCC (t m) a b
liftCallCC f g = join . lift . f $ \exit -> pure $ g (lift . exit . pure)
