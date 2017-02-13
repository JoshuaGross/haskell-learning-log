module Char where

import           Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

-- functions are functors!
fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

-- functions have applicative instances!
tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

-- functions have monad instances!
tupled' :: [Char] -> ([Char], [Char])
tupled' = do
  a <- cap
  b <- rev
  return $ (a,b)

-- functions have monad instances!
tupled'' :: [Char] -> ([Char], [Char])
tupled'' = cap >>= (\x -> rev >>= (\y -> return (x, y)))
