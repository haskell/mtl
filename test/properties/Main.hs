{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad (guard)
import Control.Monad.Accum (MonadAccum (accum, add, look))
import Control.Monad.Trans.Accum (Accum, AccumT, runAccum)
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
import Data.Functor (($>))
import Data.Functor.Identity (Identity)
import Data.Kind (Type)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink),
    Blind (Blind),
    CoArbitrary (coarbitrary),
    Fun,
    Function (function),
    Property,
    applyFun,
    chooseInt,
    forAllShrinkShow,
    functionMap,
    property,
    shrinkList,
    sized,
  )
import Test.QuickCheck.Poly (A, B)
import Test.Tasty (TestTree, adjustOption, defaultMain, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests, testProperties)
import Text.Show.Pretty (ppShow)
import Type.Reflection (Typeable, tyConName, typeRep, typeRepTyCon)

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
          accumLaws lowerWriterCPS
        ]
    ]
  where
    go :: QuickCheckTests -> QuickCheckTests
    go = max 1_000_000

-- Law generators

accumLaws ::
  forall (m :: Type -> Type) (t :: Type).
  (MonadAccum M m, Typeable m, Arbitrary t, Show t) =>
  (forall (a :: Type). (Eq a) => t -> m a -> m a -> Bool) ->
  TestTree
accumLaws runAndCompare =
  testProperties
    testName
    [ ("look *> look = look", lookLookProp),
      ("add mempty = pure ()", addMemptyProp),
      ("add x *> add y = add (x <> y)", addAddProp),
      ("add x *> look = look >>= \\w -> add x $> w <> x", addLookProp),
      ("accum (const (x, mempty)) = pure x", accumPureProp),
      ("accum f *> accum g law (too long)", accumFGProp),
      ("look = accum $ \\acc -> (acc, mempty)", lookAccumProp),
      ("add x = accum $ \\acc -> ((), x)", addAccumProp),
      ("accum f = look >>= \\acc -> let (res, v) = f acc in add v $> res", accumAddProp)
    ]
  where
    testName :: String
    testName = "MonadAccum laws for " <> typeName @(m A)
    addAccumProp :: Property
    addAccumProp = theNeedful $ \(w, x) ->
      let lhs = add x
          rhs = accum $ const ((), x)
       in property . runAndCompare w lhs $ rhs
    accumAddProp :: Property
    accumAddProp = theNeedful $ \(w, Blind (f :: M -> (A, M))) ->
      let lhs = accum f
          rhs = look >>= \acc -> let (res, v) = f acc in add v $> res
       in property . runAndCompare w lhs $ rhs
    lookLookProp :: Property
    lookLookProp = theNeedful $ \w ->
      let lhs = look *> look
          rhs = look
       in property . runAndCompare w lhs $ rhs
    addMemptyProp :: Property
    addMemptyProp = theNeedful $ \w ->
      let lhs = add mempty
          rhs = pure ()
       in property . runAndCompare w lhs $ rhs
    addAddProp :: Property
    addAddProp = theNeedful $ \(w, x, y) ->
      let lhs = add x *> add y
          rhs = add (x <> y)
       in property . runAndCompare w lhs $ rhs
    addLookProp :: Property
    addLookProp = theNeedful $ \(w, x) ->
      let lhs = add x *> look
          rhs = look >>= \w' -> add x $> w' <> x
       in property . runAndCompare w lhs $ rhs
    accumPureProp :: Property
    accumPureProp = theNeedful $ \(w, x :: A) ->
      let lhs = accum (const (x, mempty))
          rhs = pure x
       in property . runAndCompare w lhs $ rhs
    accumFGProp :: Property
    accumFGProp = theNeedful $ \(w', Blind (f :: M -> (A, M)), Blind (g :: M -> (M, M))) ->
      let lhs = accum f *> accum g
          rhs = accum $ \acc ->
            let (_, v) = f acc
                (res, w) = g (acc <> v)
             in (res, v <> w)
       in property . runAndCompare w' lhs $ rhs
    lookAccumProp :: Property
    lookAccumProp = theNeedful $ \w ->
      let lhs = look
          rhs = accum (,mempty)
       in property . runAndCompare w lhs $ rhs

-- Lowerings

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

-- Helpers

-- A type that's a 'non-specific monoid', similar to how 'A' and 'B' work in
-- QuickCheck.
--
-- We've deliberately made it _not_ commutative.
newtype M = M [Int]
  deriving (Eq, Semigroup, Monoid) via [Int]
  deriving stock (Show)

instance Arbitrary M where
  arbitrary = M . pure <$> sized (\size -> chooseInt (0, abs size))
  shrink (M xs) =
    M <$> do
      xs' <- shrinkList (const []) xs
      guard (not . null $ xs')
      pure xs'

instance CoArbitrary M where
  coarbitrary (M xs) = coarbitrary xs

instance Function M where
  function = functionMap (\(M xs) -> xs) M

newtype N = N M
  deriving (Eq, Semigroup, Monoid, Arbitrary) via M
  deriving stock (Show)

-- Avoids orphans
newtype AccumArbitrary (w :: Type) (a :: Type)
  = AccumArbitrary (Fun w (a, w), AccumT w Identity a)

instance
  (Function w, Monoid w, CoArbitrary w, Arbitrary a, Arbitrary w) =>
  Arbitrary (AccumArbitrary w a)
  where
  arbitrary = do
    f <- arbitrary
    pure . AccumArbitrary $ (f, accum . applyFun $ f)
  shrink (AccumArbitrary (f, _)) = do
    f' <- shrink f
    pure . AccumArbitrary $ (f', accum . applyFun $ f')

typeName :: forall (a :: Type). (Typeable a) => String
typeName = tyConName . typeRepTyCon $ typeRep @a

theNeedful ::
  forall (a :: Type).
  (Arbitrary a, Show a) =>
  (a -> Property) ->
  Property
theNeedful = forAllShrinkShow arbitrary shrink ppShow
