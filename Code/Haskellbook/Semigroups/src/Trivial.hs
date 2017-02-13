module Trivial where

import           Data.Monoid     (Monoid, mempty)
import           Data.Semigroup
import           Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mappend = (<>)
  mempty = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial
