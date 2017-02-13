module Main where

import           Control.Applicative
import           ReaderPractice

boop :: Num a => a -> a
boop = (*2)

doop :: Num a => a -> a
doop = (+10)

-- function composition
bip :: Integer -> Integer
bip = boop . doop

-- functor of functions (!)
bloop :: Integer -> Integer
bloop = fmap boop doop

-- applicative context (!!)
bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

-- applicative context (!!!)
duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(>3), (<8), even]

main :: IO ()
main = do
  print $ sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> ys)
  print $ summed <$> ((,) <$> xs <*> zs)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ sequenceA [(>3), (<8), even] 7
  print $ foldr (&&) True (sequA 6)
  print $ foldr (&&) True (sequA 9)
