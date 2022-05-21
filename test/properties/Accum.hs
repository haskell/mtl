{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Accum
  ( M (..),
    N (..),
    AccumArb (..),
    accumLaws,
    accumLawsCont,
  )
where

import Control.Monad (guard)
import Control.Monad.Accum (MonadAccum (accum, add, look))
import Data.Functor (($>))
import Data.Kind (Type)
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink),
    Blind (Blind),
    CoArbitrary (coarbitrary),
    Property,
    chooseInt,
    forAllShrinkShow,
    property,
    shrinkList,
    sized,
    (===),
  )
import Test.QuickCheck.Poly (A, B)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperties)
import Text.Show.Pretty (ppShow)
import Type.Reflection
  ( Typeable,
    tyConModule,
    tyConName,
    typeRep,
    typeRepTyCon,
  )

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

newtype N = N M
  deriving (Eq, Semigroup, Monoid, Arbitrary) via M
  deriving stock (Show)

newtype AccumArb (w :: Type) (a :: Type)
  = AccumArb (w -> (a, w))
  deriving (Arbitrary) via (w -> (a, w))

runAccumArb :: AccumArb w a -> w -> (a, w)
runAccumArb (AccumArb f) = f

accumLawsCont ::
  forall (m :: Type -> Type) (t :: Type).
  (MonadAccum M m, Typeable m, Arbitrary t, Show t) =>
  (forall (a :: Type). t -> m a -> (a -> AccumArb M B) -> AccumArb M B) ->
  TestTree
accumLawsCont lowerCont =
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
    addAccumProp = theNeedful $ \(w, arg, x, Blind f) ->
      let lhs = lowerCont arg (add x) f
          rhs = lowerCont arg (accum $ const ((), x)) f
       in runAccumArb lhs w === runAccumArb rhs w
    accumAddProp :: Property
    accumAddProp = theNeedful $ \(w, arg, Blind (f :: M -> (A, M)), Blind g) ->
      let lhs = lowerCont arg (accum f) g
          rhs = lowerCont arg (look >>= \acc -> let (res, v) = f acc in add v $> res) g
       in runAccumArb lhs w === runAccumArb rhs w
    lookAccumProp :: Property
    lookAccumProp = theNeedful $ \(w, arg, Blind f) ->
      let lhs = lowerCont arg look f
          rhs = lowerCont arg (accum (,mempty)) f
       in runAccumArb lhs w === runAccumArb rhs w
    lookLookProp :: Property
    lookLookProp = theNeedful $ \(w, arg, Blind f) ->
      let lhs = lowerCont arg look f
          rhs = lowerCont arg (look *> look) f
       in runAccumArb lhs w === runAccumArb rhs w
    addMemptyProp :: Property
    addMemptyProp = theNeedful $ \(w, arg, Blind f) ->
      let lhs = lowerCont arg (add mempty) f
          rhs = lowerCont arg (pure ()) f
       in runAccumArb lhs w === runAccumArb rhs w
    addAddProp :: Property
    addAddProp = theNeedful $ \(w, arg, x, y, Blind f) ->
      let lhs = lowerCont arg (add x *> add y) f
          rhs = lowerCont arg (add (x <> y)) f
       in runAccumArb lhs w === runAccumArb rhs w
    addLookProp :: Property
    addLookProp = theNeedful $ \(w, arg, x, Blind f) ->
      let lhs = lowerCont arg (add x *> look) f
          rhs = lowerCont arg (look >>= \w' -> add x $> w' <> x) f
       in runAccumArb lhs w === runAccumArb rhs w
    accumPureProp :: Property
    accumPureProp = theNeedful $ \(w, arg, x :: A, Blind f) ->
      let lhs = lowerCont arg (accum (const (x, mempty))) f
          rhs = lowerCont arg (pure x) f
       in runAccumArb lhs w === runAccumArb rhs w
    accumFGProp :: Property
    accumFGProp = theNeedful $ \(w', arg, Blind (f :: M -> (A, M)), Blind (g :: M -> (M, M)), Blind h) ->
      let lhs = lowerCont arg (accum f *> accum g) h
          rhs =
            lowerCont
              arg
              ( accum $ \acc ->
                  let (_, v) = f acc
                      (res, w) = g (acc <> v)
                   in (res, v <> w)
              )
              h
       in runAccumArb lhs w' === runAccumArb rhs w'

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

-- Helpers

typeName :: forall (a :: Type). (Typeable a) => String
typeName =
  let ourTyCon = typeRepTyCon $ typeRep @ a
   in tyConModule ourTyCon <> "." <> tyConName ourTyCon

theNeedful ::
  forall (a :: Type).
  (Arbitrary a, Show a) =>
  (a -> Property) ->
  Property
theNeedful = forAllShrinkShow arbitrary shrink ppShow
