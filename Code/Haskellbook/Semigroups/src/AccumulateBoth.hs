module AccumulateBoth where

import           Data.Semigroup
import           Test.QuickCheck (Arbitrary, arbitrary, oneof)
import           Validation

newtype AccumulateBoth a b = AccumulateBoth (Validation a b) deriving (Eq, Show)

-- accumulate successes or failures. preference for failures
instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  (AccumulateBoth (Success x)) <> (AccumulateBoth (Success y)) = AccumulateBoth (Success (x <> y))
  (AccumulateBoth (Failure x)) <> (AccumulateBoth (Failure y)) = AccumulateBoth (Failure (x <> y))
  x@(AccumulateBoth (Failure _)) <> _ = x
  _ <> x = x

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof $ fmap return [AccumulateBoth (Failure a), AccumulateBoth (Success b)]
