module Vigenere where

import           Data.Char

vigenereShiftValue :: Char -> Int
vigenereShiftValue k
  | k >= 'a' && k <= 'z' = ord k - ord 'a'
  | k >= 'A' && k <= 'Z' = ord k - ord 'A'
  | otherwise = 0

-- Key -> text
vigenere' :: Int -> String -> String -> String
vigenere' _ _ "" = ""
vigenere' m key (c:cs)
  | c >= 'a' && c <= 'z' = (shiftChr c 'a') : cont'
  | c >= 'A' && c <= 'Z' = (shiftChr c 'A') : cont'
  | otherwise = c : cont
  where cont  = vigenere' m key' cs
        cont' = vigenere' m (cr (tail key')) cs
        key' = cr key
        cr = concat . repeat
        shiftValue = vigenereShiftValue (head key)
        shiftChr c base = chr ((ord base) + (mod ((ord c) - (ord base) + (shiftValue * m)) 26))

vigenere :: String -> String -> String
vigenere = vigenere' 1

unvigenere :: String -> String -> String
unvigenere = vigenere' (-1)
