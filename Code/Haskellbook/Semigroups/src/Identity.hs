{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Identity where

import           Data.Monoid              (Monoid, mempty)
import           Data.Semigroup
import           GHC.Generics
import           Test.QuickCheck
import           Test.QuickCheck.Checkers (EqProp, eq, (=-=))

data Identity a = Identity a deriving (Eq, Show, Generic, CoArbitrary)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance (Monoid a, Semigroup a) => Monoid (Identity a) where
  mappend = (<>)
  mempty = Identity mempty

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity (f x)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance Eq a => EqProp (Identity a) where
  (=-=) = eq
