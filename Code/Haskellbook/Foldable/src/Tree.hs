module Tree where

import           Data.Semigroup
import           Test.QuickCheck
import           Test.QuickCheck.Checkers (EqProp, eq, (=-=))

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node xs x xs') = Node (fmap f xs) (f x) (fmap f xs')

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node xs x xs') = foldMap f xs `mappend` f x `mappend` foldMap f xs'

-- λ> traverse (Just . (+10)) (Node (Leaf 1) 2 (Leaf 3))
instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Node xs x xs') = Node <$> traverse f xs <*> f x <*> traverse f xs'

-- λ> sample (arbitrary :: Gen (Tree Int))
instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    frequency [(1, return Empty), (2, return $ Leaf a), (3, return $ Node b c d)]

instance Eq a => EqProp (Tree a) where
  (=-=) = eq
