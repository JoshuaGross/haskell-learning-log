module Validation where

import           Data.Semigroup
import           Test.QuickCheck (Arbitrary, arbitrary, oneof)

data Validation a b = Failure a | Success b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Validation a b) where
  (Success x) <> (Success y) = Success (x <> y)
  (Failure x) <> (Failure y) = Failure (x <> y)
  (Failure x) <> _ = Failure x
  _ <> (Failure x) = Failure x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof $ fmap return [Success a, Failure b]
