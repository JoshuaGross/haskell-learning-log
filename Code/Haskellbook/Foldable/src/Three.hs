module Three where

import           Data.Monoid              (Monoid, mempty)
import           Data.Semigroup
import           Test.QuickCheck
import           Test.QuickCheck.Checkers (EqProp, eq, (=-=))

-- Test:
-- λ> ((Three 1 2 (+1)) <*> (Three 1 2 3)) :: (Three (Sum Int) (Sum Int) (Sum Int))
-- Three (Sum {getSum = 2}) (Sum {getSum = 4}) (Sum {getSum = 4})

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

instance (Monoid a, Semigroup a, Monoid b, Semigroup b, Monoid c, Semigroup c) => Monoid (Three a b c) where
  mappend = (<>)
  mempty = Three mempty mempty mempty

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Semigroup a, Semigroup b, Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (Three a' b' f) <*> (Three a b c) = Three (a' <> a) (b' <> b) (f c)

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = (Three a b) <$> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq
