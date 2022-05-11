{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad.Accum (MonadAccum (add, look))
import Control.Monad.Trans.Accum (AccumT, runAccum)
import Data.Functor (($>))
import Data.Functor.Identity (Identity)
import Data.Kind (Type)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink),
    Property,
    chooseInt,
    forAllShrinkShow,
    sized,
    (===),
  )
import Test.QuickCheck.Poly (A)
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
        [ accumLaws lowerBaseline
        ]
    ]
  where
    go :: QuickCheckTests -> QuickCheckTests
    go = max 1_000_000

-- Law generators

accumLaws ::
  forall (m :: Type -> Type).
  (MonadAccum M m, Typeable m) =>
  (forall (a :: Type). m a -> M -> (a, M)) ->
  TestTree
accumLaws lower =
  testProperties
    testName
    [ ("look *> look = look", lookLookProp),
      ("add mempty = pure ()", addMemptyProp),
      ("add x *> add y = add (x <> y)", addAddProp),
      ("add x *> look = look >>= \\w -> add x $> w <> x", addLookProp)
    ]
  where
    testName :: String
    testName = "MonadAccum laws for " <> typeName @(m A)
    lookLookProp :: Property
    lookLookProp = theNeedful $ \w ->
      let lhs = look *> look
          rhs = look
       in lower lhs w === lower rhs w
    addMemptyProp :: Property
    addMemptyProp = theNeedful $ \w ->
      let lhs = add mempty
          rhs = pure ()
       in lower lhs w === lower rhs w
    addAddProp :: Property
    addAddProp = theNeedful $ \(w, x, y) ->
      let lhs = add x *> add y
          rhs = add (x <> y)
       in lower lhs w === lower rhs w
    addLookProp :: Property
    addLookProp = theNeedful $ \(w, x) ->
      let lhs = add x *> look
          rhs = look >>= \w' -> add x $> w' <> x
       in lower lhs w === lower rhs w

-- Helpers

-- A type that's a 'non-specific monoid', similar to how 'A' and 'B' work in
-- QuickCheck.
--
-- We've deliberately made it _not_ commutative.
newtype M = M [Int]
  deriving (Eq, Semigroup, Monoid) via [Int]
  deriving stock (Show)

instance Arbitrary M where
  arbitrary = do
    x <- sized $ \size -> chooseInt (0, abs size)
    pure . M . pure $ x

lowerBaseline ::
  forall (a :: Type) (w :: Type).
  AccumT w Identity a ->
  w ->
  (a, w)
lowerBaseline = runAccum

typeName :: forall (a :: Type). (Typeable a) => String
typeName = tyConName . typeRepTyCon $ typeRep @a

theNeedful ::
  forall (a :: Type).
  (Arbitrary a, Show a) =>
  (a -> Property) ->
  Property
theNeedful = forAllShrinkShow arbitrary shrink ppShow
