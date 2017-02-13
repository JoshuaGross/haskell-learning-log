module Main where

import           GHC.Stack

recursiveFn :: Int -> IO [String]
recursiveFn x
  | x < 100 = recursiveFn (x + 1)
  | x == 100 = currentCallStack

main :: IO ()
main = do
  stack <- recursiveFn 0
  mapM_ putStrLn stack
