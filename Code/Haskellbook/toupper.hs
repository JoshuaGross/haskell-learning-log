module ToUpper where

import           Data.Char

firstToUpper :: [Char] -> [Char]
firstToUpper (x:xs) = (toUpper x) : xs
