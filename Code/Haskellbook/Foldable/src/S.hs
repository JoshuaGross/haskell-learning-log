module S where

--import           Data.Semigroup
import           Test.QuickCheck
import           Test.QuickCheck.Checkers (EqProp, eq, (=-=))

data S n a = S (n a) a deriving (Eq, Show)

-- λ> fmap (+1) (S [1,2,3] 1)
instance Functor n => Functor (S n) where
  fmap f (S n a) = S (fmap f n) (f a)

-- λ> foldMap Sum (S [1,2,3] 1)
-- λ> foldMap Product (S [1,2,3] 1)
instance Foldable n => Foldable (S n) where
  foldMap f (S n a) = foldMap f n `mappend` f a

-- λ> traverse Just (S [1,2,3] 1)
-- λ> traverse (Just . (+10)) (S [1,2,3] 1)
-- λ> traverse (Just . Sum . (+10)) (S [1,2,3] 1)
-- λ> (traverse (Just . (+1)) [1,2,3])
-- λ> fmap S (traverse Just [1,2,3])
instance Traversable n => Traversable (S n) where
  traverse f (S n a) = S <$> traverse f n <*> f a

instance (Applicative n, Arbitrary a) => Arbitrary (S n a) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    return $ S (pure a) a'

instance (Eq (n a), Eq a) => EqProp (S n a) where
  (=-=) = eq
