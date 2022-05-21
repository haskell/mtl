{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Accum (AccumArb (AccumArb), M, N, accumLaws, accumLawsCont)
import Control.Monad.Trans.Accum (Accum, AccumT (AccumT), accum, runAccum)
import Control.Monad.Trans.Cont (ContT, runContT)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Identity (IdentityT, runIdentityT)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import qualified Control.Monad.Trans.RWS.CPS as RWSCPS
import qualified Control.Monad.Trans.RWS.Lazy as RWSLazy
import qualified Control.Monad.Trans.RWS.Strict as RWSStrict
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Trans.State.Lazy as StateLazy
import qualified Control.Monad.Trans.State.Strict as StateStrict
import qualified Control.Monad.Trans.Writer.CPS as WriterCPS
import qualified Control.Monad.Trans.Writer.Lazy as WriterLazy
import qualified Control.Monad.Trans.Writer.Strict as WriterStrict
import Data.Functor.Identity (Identity, runIdentity)
import Data.Kind (Type)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Test.QuickCheck.Poly (A, B)
import Test.Tasty (adjustOption, defaultMain, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests)

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain . adjustOption go . testGroup "Laws" $
    [ testGroup
        "Accum"
        [ accumLaws lowerBase,
          accumLaws lowerMaybe,
          accumLaws lowerExcept,
          accumLaws lowerIdentity,
          accumLaws lowerRWSLazy,
          accumLaws lowerRWSStrict,
          accumLaws lowerRWSCPS,
          accumLaws lowerReader,
          accumLaws lowerStateLazy,
          accumLaws lowerStateStrict,
          accumLaws lowerWriterLazy,
          accumLaws lowerWriterStrict,
          accumLaws lowerWriterCPS,
          accumLawsCont lowerCont
        ]
    ]
  where
    go :: QuickCheckTests -> QuickCheckTests
    go = max 1_000_000

-- Lowerings

lowerCont ::
  forall (a :: Type).
  () ->
  ContT B (Accum M) a ->
  (a -> AccumArb M B) ->
  AccumArb M B
lowerCont _ comp handler =
  demote . runContT comp $ (promote . handler)

promote ::
  forall (w :: Type) (a :: Type).
  AccumArb w a ->
  Accum w a
promote (AccumArb f) = accum f

demote ::
  forall (w :: Type) (a :: Type).
  Accum w a ->
  AccumArb w a
demote (AccumT f) = AccumArb $ \w -> runIdentity . f $ w

lowerBase ::
  forall (a :: Type).
  (Eq a) =>
  M ->
  AccumT M Identity a ->
  AccumT M Identity a ->
  Bool
lowerBase w lhs rhs = runAccum lhs w == runAccum rhs w

lowerMaybe ::
  forall (a :: Type).
  (Eq a) =>
  M ->
  MaybeT (Accum M) a ->
  MaybeT (Accum M) a ->
  Bool
lowerMaybe w lhs rhs =
  let leftRun = runAccum (runMaybeT lhs) w
      rightRun = runAccum (runMaybeT rhs) w
   in leftRun == rightRun

lowerExcept ::
  forall (a :: Type).
  (Eq a) =>
  M ->
  ExceptT A (Accum M) a ->
  ExceptT A (Accum M) a ->
  Bool
lowerExcept w lhs rhs =
  let leftRun = runAccum (runExceptT lhs) w
      rightRun = runAccum (runExceptT rhs) w
   in leftRun == rightRun

lowerIdentity ::
  forall (a :: Type).
  (Eq a) =>
  M ->
  IdentityT (Accum M) a ->
  IdentityT (Accum M) a ->
  Bool
lowerIdentity w lhs rhs =
  let leftRun = runAccum (runIdentityT lhs) w
      rightRun = runAccum (runIdentityT rhs) w
   in leftRun == rightRun

lowerRWSLazy ::
  forall (a :: Type).
  (Eq a) =>
  (M, A, B) ->
  RWSLazy.RWST A M B (Accum M) a ->
  RWSLazy.RWST A M B (Accum M) a ->
  Bool
lowerRWSLazy (w, r, s) lhs rhs =
  let leftRun = runAccum (RWSLazy.runRWST lhs r s) w
      rightRun = runAccum (RWSLazy.runRWST rhs r s) w
   in leftRun == rightRun

lowerRWSStrict ::
  forall (a :: Type).
  (Eq a) =>
  (M, A, B) ->
  RWSStrict.RWST A M B (Accum M) a ->
  RWSStrict.RWST A M B (Accum M) a ->
  Bool
lowerRWSStrict (w, r, s) lhs rhs =
  let leftRun = runAccum (RWSStrict.runRWST lhs r s) w
      rightRun = runAccum (RWSStrict.runRWST rhs r s) w
   in leftRun == rightRun

lowerRWSCPS ::
  forall (a :: Type).
  (Eq a) =>
  (M, A, B) ->
  RWSCPS.RWST A M B (Accum M) a ->
  RWSCPS.RWST A M B (Accum M) a ->
  Bool
lowerRWSCPS (w, r, s) lhs rhs =
  let leftRun = runAccum (RWSCPS.runRWST lhs r s) w
      rightRun = runAccum (RWSCPS.runRWST rhs r s) w
   in leftRun == rightRun

lowerReader ::
  forall (a :: Type).
  (Eq a) =>
  (M, A) ->
  ReaderT A (Accum M) a ->
  ReaderT A (Accum M) a ->
  Bool
lowerReader (w, r) lhs rhs =
  let leftRun = runAccum (runReaderT lhs r) w
      rightRun = runAccum (runReaderT rhs r) w
   in leftRun == rightRun

lowerStateLazy ::
  forall (a :: Type).
  (Eq a) =>
  (M, A) ->
  StateLazy.StateT A (Accum M) a ->
  StateLazy.StateT A (Accum M) a ->
  Bool
lowerStateLazy (w, s) lhs rhs =
  let leftRun = runAccum (StateLazy.runStateT lhs s) w
      rightRun = runAccum (StateLazy.runStateT rhs s) w
   in leftRun == rightRun

lowerStateStrict ::
  forall (a :: Type).
  (Eq a) =>
  (M, A) ->
  StateStrict.StateT A (Accum M) a ->
  StateStrict.StateT A (Accum M) a ->
  Bool
lowerStateStrict (w, s) lhs rhs =
  let leftRun = runAccum (StateStrict.runStateT lhs s) w
      rightRun = runAccum (StateStrict.runStateT rhs s) w
   in leftRun == rightRun

lowerWriterLazy ::
  forall (a :: Type).
  (Eq a) =>
  M ->
  WriterLazy.WriterT N (Accum M) a ->
  WriterLazy.WriterT N (Accum M) a ->
  Bool
lowerWriterLazy w lhs rhs =
  let leftRun = runAccum (WriterLazy.runWriterT lhs) w
      rightRun = runAccum (WriterLazy.runWriterT rhs) w
   in leftRun == rightRun

lowerWriterStrict ::
  forall (a :: Type).
  (Eq a) =>
  M ->
  WriterStrict.WriterT N (Accum M) a ->
  WriterStrict.WriterT N (Accum M) a ->
  Bool
lowerWriterStrict w lhs rhs =
  let leftRun = runAccum (WriterStrict.runWriterT lhs) w
      rightRun = runAccum (WriterStrict.runWriterT rhs) w
   in leftRun == rightRun

lowerWriterCPS ::
  forall (a :: Type).
  (Eq a) =>
  M ->
  WriterCPS.WriterT N (Accum M) a ->
  WriterCPS.WriterT N (Accum M) a ->
  Bool
lowerWriterCPS w lhs rhs =
  let leftRun = runAccum (WriterCPS.runWriterT lhs) w
      rightRun = runAccum (WriterCPS.runWriterT rhs) w
   in leftRun == rightRun
