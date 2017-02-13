module BoolConj where

import           Data.Monoid     (Monoid, mempty)
import           Data.Semigroup
import           Test.QuickCheck

data BoolConj = BoolConj Bool deriving (Show, Eq)

instance Semigroup BoolConj where
  (BoolConj x) <> (BoolConj y) = BoolConj (x && y)

instance Monoid BoolConj where
  mappend = (<>)
  mempty = BoolConj True

instance Arbitrary BoolConj where
  arbitrary = do
    a <- arbitrary
    return $ BoolConj a
