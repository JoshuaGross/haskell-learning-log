module Main where

import qualified Data.Map            as M
import           Morse
import           Test.QuickCheck
import           Test.QuickCheck.Gen (oneof)

data Trivial = Trivial deriving (Show, Eq)

data Identity a = Identity a deriving (Show, Eq)

data Pair a b = Pair a b deriving (Show, Eq)

data Sum a b = First a | Second b deriving (Eq, Show)

allowedChars :: [Char]
allowedChars = M.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = M.elems letterToMorse

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse

prop_thereAndBackAgain :: Property
prop_thereAndBackAgain =
  forAll charGen
  (\c -> ((charToMorse c) >>= morseToChar) == Just c)

-- try it out: `sample trivialGen`
trivialGen :: Gen Trivial
trivialGen = return Trivial

instance Arbitrary Trivial where
  arbitrary = trivialGen

-- try it out: `sample (identityGen :: Gen (Identity Int))`
-- try it out: `sample (identityGen :: Gen (Identity String))`
-- try it out: `sample (identityGen :: Gen (Identity (Maybe Char)))`
identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return $ Identity a

-- try it out: `sample (pairGen :: Gen (Pair Int Int))`
-- try it out: `sample (pairGen :: Gen (Pair String String))`
pairGen :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
pairGen = do
  a <- arbitrary
  b <- arbitrary
  return (Pair a b)

-- try it out: `sample (sumGen :: Gen (Sum Int Char))`
sumGen :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGen = do
  a <- arbitrary
  b <- arbitrary
  oneof [return (First a), return (Second b)]

main :: IO ()
main = quickCheck prop_thereAndBackAgain
