module List where

import           Data.Semigroup
import           Test.QuickCheck
import           Test.QuickCheck.Checkers (EqProp, eq, (=-=))

data List a = Nil | Cons a (List a) deriving (Show, Eq)

instance Semigroup (List a) where
  (<>) (Cons a as) bs = Cons a (as <> bs)
  (<>) Nil a = a

instance Monoid (List a) where
  mempty = Nil
  mappend = (<>)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure = flip Cons Nil
  fs <*> xs = foldr (\f b -> b <> fmap f xs) Nil fs

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = f x `mappend` (foldMap f xs)

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ Cons a b, return $ Cons a Nil]

instance Eq a => EqProp (List a) where
  (=-=) = eq
