module Constant where

import           Data.Semigroup

data Constant a b = Constant { getConstant :: a } deriving (Eq, Show, Ord)

instance Functor (Constant a) where
  fmap _ (Constant { getConstant=a }) = Constant a

instance (Semigroup a, Monoid a) => Applicative (Constant a) where
  pure _ = Constant mempty
  (Constant { getConstant=a }) <*> (Constant { getConstant=b }) = Constant (a <> b)
