module Linguists where

import           Data.List

splitStringBy :: Char -> String -> [String]
splitStringBy c = foldl (\ss s -> if s == c then ss++[""] else (take (length ss - 1) ss) ++ [last ss ++ [s]]) [""]

splitWords :: String -> [String]
splitWords = splitStringBy ' '

replaceThe :: String -> String
replaceThe = concat . intersperse " " . map (\s -> if s == "the" then "a" else s) . splitWords

vowels :: String
vowels = "aeiou"

isVowel :: Char -> Bool
isVowel = flip elem vowels

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = count' . splitWords
  where
    count' :: [String] -> Integer
    count' ([]) = 0
    count' ("the":[]) = 0
    count' ("the":((x:xs):ys))
      | isVowel x = 1 + count' ys
      | otherwise = 0 + count' ys
    count' (_:ys) = 0 + count' ys

countVowels :: String -> Integer
countVowels "" = 0
countVowels (x:xs)
  | isVowel x = 1 + countVowels xs
  | otherwise = 0 + countVowels xs

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord w = if c >= v then Just (Word' w) else Nothing
  where l = length w
        v = length $ filter isVowel w
        c = length $ filter (not . isVowel) w
