{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Three where

import           Data.Monoid              (Monoid, mempty)
import           Data.Semigroup
import           GHC.Generics
import           Test.QuickCheck
import           Test.QuickCheck.Checkers (EqProp, eq, (=-=))

data Three a b c = Three a b c deriving (Eq, Show, Generic, CoArbitrary)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

instance (Monoid a, Semigroup a, Monoid b, Semigroup b, Monoid c, Semigroup c) => Monoid (Three a b c) where
  mappend = (<>)
  mempty = Three mempty mempty mempty

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq
