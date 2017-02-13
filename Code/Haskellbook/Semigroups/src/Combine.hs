module Combine where

import           Data.Monoid    (Monoid, mempty)
import           Data.Semigroup

newtype Combine a b = Combine { unCombine :: a -> b }

-- mappend of two functions (f and g) of type `a -> b`
-- is defined as: `f a <> g a`, therefore `b` must be a Semigroup.
instance Semigroup b => Semigroup (Combine a b) where
  Combine { unCombine=f } <> Combine { unCombine=g } = Combine (f <> g)

instance (Monoid b, Semigroup b) => Monoid (Combine a b) where
  mappend = (<>)
  mempty = Combine $ const mempty

-- no idea on arbitrary / coarbitrary, should revisit later

-- > :set -XScopedTypeVariables
-- > let x :: Int -> Int = \x' -> x'
-- > :t Combine x
-- Combine x :: Combine Int Int

-- instance (Arbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
--   arbitrary = do
--     a <- arbitrary
--     b <- arbitrary
--     elements [Fst a, Snd b]
