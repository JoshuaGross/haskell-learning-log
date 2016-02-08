module Lib
    ( someFunc
    ) where

import           Control.Parallel.Strategies

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

x = 25
y = 35

someFunc :: (Integer, Integer)
someFunc = runEval $ do
  a <- rpar (fib x)
  b <- rpar (fib y)
  return (a, b)
