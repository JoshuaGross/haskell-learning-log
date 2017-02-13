module ReaderPractice where

import           Control.Applicative
import           Data.Maybe

x :: [Integer]
x = [1, 2, 3]

y :: [Integer]
y = [4, 5, 6]

z :: [Integer]
z = [7, 8, 9]

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' = flip lookup $ zip x z

z'' :: Integer -> Integer -> Maybe Integer
z'' a = fmap (+a) . flip lookup (zip x z)

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

-- (,) :: a -> b -> (a, b)
-- (<$>) :: Functor f => (a -> b) -> f a -> f b
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
--
-- z' :: Integer -> Maybe Integer
--
-- (,) is a Functor but is not an Applicative.
-- ((->) a) is a Functor and an Applicative.
-- Maybe is a Functor and an Applicative.
--
-- Apply (,) to <$>: ((,) <$>) :: Functor f => f a -> f (b -> (a, b))
-- Apply z': ((,) <$> z') :: Integer -> b -> (Maybe Integer, b)
-- Apply <*>: ((,) <$> z' <*>) :: (Integer -> a) -> Integer -> (Maybe Integer, a)
-- Apply z': ((,) <$> z' <*> z') :: Integer -> (Maybe Integer, Maybe Integer)
--
-- Other experiments:
-- ((,) 5 <$>) :: (Num a, Functor f) => f b -> f (a, b)
-- ((,) (5 :: Int) <$>) :: Functor f => f b -> f (Int, b)
-- ((,) (5 :: Int) <$> z') :: Integer -> (Int, Maybe Integer)
x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = (,) <$> z' <*> z'

x3' :: Integer -> (Maybe Integer, Maybe Integer, Maybe Integer)
x3' = (,,) <$> z' <*> z' <*> z'

-- Take a number; then look up the next number, and attempt to add 1st number to
-- result of 2nd. Return both tupled.
-- ((,) <$> z'')         :: Integer -> b -> (Integer -> Maybe Integer, b)
-- ((,) <$> z'' <*>)     :: (Integer -> a) -> Integer -> (Integer -> Maybe Integer, a)
-- ((,) <$> z'' <*> z'') :: Integer -> (Integer -> Maybe Integer, Integer -> Maybe Integer)
-- x3' :: Integer -> Integer -> (Maybe Integer, Maybe Integer)
-- x3' = (,) <$> z'' <*> z''

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = (&&) <$> (>3) <*> (<8)
