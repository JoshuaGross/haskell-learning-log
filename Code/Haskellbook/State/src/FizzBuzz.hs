module FizzBuzz where

import           Control.Monad
import           Control.Monad.Trans.State
import qualified Data.DList                as DL

fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5  == 0 = "Fizz"
  | n `mod` 5  == 0 = "Fuzz"
  | otherwise = show n

fizzBuzzList :: [Integer] -> (DL.DList String)
fizzBuzzList list = execState (mapM_ addResult list) DL.empty

addResult :: Integer -> State (DL.DList String) (DL.DList String)
addResult n = do
  xs <- get
  let result = fizzBuzz n
  let xs' = DL.snoc xs result
  put xs'
  return xs'

main :: IO ()
main = mapM_ putStrLn $ fizzBuzzList [1..100]
