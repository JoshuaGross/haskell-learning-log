module BoolDisj where

import           Data.Monoid     (Monoid, mempty)
import           Data.Semigroup
import           Test.QuickCheck

data BoolDisj = BoolDisj Bool deriving (Show, Eq)

instance Semigroup BoolDisj where
  (BoolDisj x) <> (BoolDisj y) = BoolDisj (x || y)

instance Monoid BoolDisj where
  mappend = (<>)
  mempty = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = do
    a <- arbitrary
    return $ BoolDisj a
