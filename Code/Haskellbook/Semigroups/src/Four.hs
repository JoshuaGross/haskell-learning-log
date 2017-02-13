{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Four where

import           Data.Monoid              (Monoid, mempty)
import           Data.Semigroup
import           GHC.Generics
import           Test.QuickCheck
import           Test.QuickCheck.Checkers (EqProp, eq, (=-=))

data Four a b c d = Four a b c d deriving (Eq, Show, Generic, CoArbitrary)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four a b c d) <> (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') (d <> d')

instance (Monoid a, Semigroup a, Monoid b, Semigroup b, Monoid c, Semigroup c, Monoid d, Semigroup d) => Monoid (Four a b c d) where
  mappend = (<>)
  mempty = Four mempty mempty mempty mempty

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq
