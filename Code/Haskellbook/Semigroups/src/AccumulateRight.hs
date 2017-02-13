module AccumulateRight where

import           Data.Semigroup
import           Test.QuickCheck (Arbitrary, arbitrary, oneof)
import           Validation

newtype AccumulateRight a b = AccumulateRight (Validation a b) deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
  (AccumulateRight (Success x)) <> (AccumulateRight (Success y)) = AccumulateRight (Success (x <> y))
  x@(AccumulateRight (Success _)) <> _ = x
  _ <> x@(AccumulateRight _) = x

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof $ fmap return [AccumulateRight (Failure a), AccumulateRight (Success b)]
