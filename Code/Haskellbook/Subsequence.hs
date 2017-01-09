module Subsequence where

import           Data.Char
import           Data.List hiding (isSubsequenceOf)

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] [] = True
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf ss@(x:xs) (y:ys) =
  (x == y && isSubsequenceOf xs ys) || (isSubsequenceOf ss ys)

splitStringBy :: Char -> String -> [String]
splitStringBy c = foldl (\ss s -> if s == c then ss++[""] else (take (length ss - 1) ss) ++ [last ss ++ [s]]) [""]

splitWords :: String -> [String]
splitWords = splitStringBy ' '

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map (\s -> (s, firstToUpper s)) . splitWords

firstToUpper :: String -> String
firstToUpper "" = ""
firstToUpper (x:xs)
  | x /= ' '  = (toUpper x) : xs
  | otherwise = x : (firstToUpper xs)

capitalizeSentences :: String -> String
capitalizeSentences = concat . intersperse "." . map firstToUpper . splitStringBy '.'
