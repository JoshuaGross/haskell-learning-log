{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module ListAp where

import           Data.Semigroup
import           GHC.Generics
import           Test.QuickCheck          (Arbitrary, CoArbitrary, arbitrary,
                                           oneof)
import           Test.QuickCheck.Checkers (EqProp, eq, (=-=))

data List a = Nil | Cons a (List a) deriving (Eq, Show, Generic, CoArbitrary)

instance Semigroup (List a) where
  (<>) (Cons a as) bs = Cons a (as <> bs)
  (<>) Nil a = a

instance Monoid (List a) where
  mempty = Nil
  mappend = (<>)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons y ys) = Cons (f y) (fmap f ys)

instance Foldable List where
  foldr f b (Cons y ys) = foldr f (f y b) ys
  foldr _ b _ = b

instance Applicative List where
  pure = flip Cons Nil
  -- 1:
  --fs <*> xs = foldr (\f -> flip mappend (foldr (flip mappend . pure . f) Nil xs)) Nil fs
  -- 2:
  --(<*>) Nil ca = ca
  --(<*>) (Cons f b) ca = fmap f ca <> (b <*> ca)
  -- 3:
  (<*>) fs xs = foldr (\f b -> b <> fmap f xs) Nil fs

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ Cons a b, return $ Cons a Nil]

instance Eq a => EqProp (List a) where
  (=-=) = eq

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' 0 _ = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)

length' :: List a -> Int
length' = foldr (\_ b -> b + 1) 0

x :: List Int
x = Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))

x' :: List Int
x' = (+10) <$> x

x'' :: List Int
x'' = Cons (+1) (Cons (*5) Nil) <*> x

fns :: List (Int -> Int)
fns = Cons (+1) (Cons (*2) Nil)

infList' :: Int -> List (Int)
infList' n = Cons n (infList' (n + 1))

infList :: List (Int)
infList = infList' 0

y :: List Int
y = Cons 1 (Cons 2 Nil)

apTest :: List Int
apTest = fns <*> y

apTestOk :: Bool
apTestOk = apTest == (Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil))))
