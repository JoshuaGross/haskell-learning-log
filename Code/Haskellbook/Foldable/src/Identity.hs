module Identity where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers (EqProp, eq, (=-=))

data Identity a = Identity a deriving (Show, Eq)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x

instance Traversable Identity where
  -- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse f (Identity x) = Identity <$> f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance Eq a => EqProp (Identity a) where
  (=-=) = eq
