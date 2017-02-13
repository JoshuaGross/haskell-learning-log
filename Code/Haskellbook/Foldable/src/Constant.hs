module Constant where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers (EqProp, eq, (=-=))

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  (Constant x) <*> (Constant y) = Constant (x `mappend` y)

instance Traversable (Constant a) where
  traverse _ (Constant x) = Constant <$> pure x

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = do
    a <- arbitrary
    return $ Constant a

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq
