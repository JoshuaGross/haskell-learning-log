module Main where

import           Control.Monad

-- exercise: write bind in terms of `fmap` and `join`.
-- λ> :t join
-- join :: Monad m => m (m a) -> m a
-- λ> :t fmap
-- fmap :: Functor f => (a -> b) -> f a -> f b
--
-- Remember that Monad implies Applicative implies Functor.
bind :: Monad m => (a -> m b) -> m a -> m b
bind = (join .) . fmap


twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else [x*x]

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else []

main :: IO ()
main = do
  putStrLn "hello world"
