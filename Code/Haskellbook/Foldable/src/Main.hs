module Main where

import           Constant
import           Identity
import           List
import           Optional
import           S
import           Three
import           Tree

import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes


-- filterF        :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
-- foldMap        :: Monoid m => (a -> m) -> t a -> m
-- (foldMap pure) :: (Monoid (f a), Foldable t, Applicative f) => t a -> f a
filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap $ \x -> if f x then pure x else mempty

-- Orphan!
instance Arbitrary a => Arbitrary (Sum a) where
  arbitrary = Sum <$> arbitrary

main :: IO ()
main = do
  quickBatch (traversable (undefined :: [(Int, Int, [Int])]))
  quickBatch (traversable (undefined :: (Identity (Int, Int, [Int]))))
  quickBatch (traversable (undefined :: ((Constant (Sum Int)) (Int, Int, [Int]))))
  quickBatch (traversable (undefined :: ((Constant Int) (Int, Int, [Int]))))
  quickBatch (traversable (undefined :: (Optional (Int, Int, [Int]))))
  quickBatch (traversable (undefined :: (List (Int, Int, [Int]))))
  quickBatch (traversable (undefined :: ((Three (Sum Int) (Sum Int)) (Int, Int, [Int]))))
  quickBatch (traversable (undefined :: ((S []) (Int, Int, [Int]))))
  quickBatch (traversable (undefined :: (Tree (Int, Int, [Int]))))
