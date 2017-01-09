module Ch1010ex1 (
    stops
  , vowels
  , nouns
  , verbs
  , allWords
  , allWordsPrefixP
  ) where

import           Combinatorial (comboOfN)
import           Data.Monoid

stops :: [Char]
stops  = "pbtdkg"

vowels :: [Char]
vowels = "aeiou"

nouns :: [String]
nouns = ["bird", "dog", "cat", "car", "Elon Musk", "the dying of the light", "my cousin vinny"]

verbs :: [String]
verbs = ["hits", "runs", "starts", "stops", "kills", "shouts at", "calls", "rolls eyes at"]

-- returns all three-character combinations of stop-vowel-stop
-- zip all beginnings with each ending vowels
allWords :: [String]
allWords = comboOfN mempty (map (map return) [stops, vowels, stops])

allWordsPrefixP :: [String]
allWordsPrefixP = filter (\(x:xs) -> x == 'p') allWords

-- returns all sentences composed of noun-verb-noun
allSentences :: [String]
allSentences = comboOfN " " [nouns, verbs, nouns]

-- get all combos of N lists of elements
-- From Combinatorial.hs
comboOfN :: Monoid a => a -> [[a]] -> [a]
comboOfN sep (z:zs) = foldl ((joinCombinations .) . makeCombinations) z zs
  where
    makeCombinations x y = zip (take (length y) (repeat x)) y
    joinCombinations = concatMap $ \(xs, y) -> map (\x -> x <> sep <> y) xs
