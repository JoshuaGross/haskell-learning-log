module Cipher (caesar, uncaesar) where

import           Data.Char

minLower = (ord 'a')
maxLower = (ord 'z')

minUpper = (ord 'A')
maxUpper = (ord 'Z')

caesar :: Int -> [Char] -> [Char]
caesar n (x:xs) = (caesarCh n x) : (caesar n xs)
caesar n [] = ""

uncaesar :: Int -> [Char] -> [Char]
uncaesar n = caesar (negate n)

caesarCh :: Int -> Char -> Char
caesarCh n x
  | isUpper x = chr (minUpper + (mod (x' - minUpper + n) 26))
  | isLower x = chr (minLower + (mod (x' - minLower + n) 26))
  | otherwise = x
  where x' = ord x
