module Optional where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers (EqProp, eq, (=-=))

data Optional a = Nada | Yep a deriving (Eq, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep x) = Yep $ f x

instance Applicative Optional where
  pure = Yep
  (Yep f) <*> (Yep x) = Yep $ f x
  _ <*> _ = Nada

instance Foldable Optional where
  foldMap f (Yep x) = f x
  foldMap _ _ = mempty

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep x) = Yep <$> f x

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    oneof [return $ Yep a, return $ Nada]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq
