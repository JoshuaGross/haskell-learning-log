module Addition where

import           Test.Hspec
import           Test.QuickCheck

sayHello :: IO ()
sayHello = putStrLn "hey there"

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genTriple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genTriple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

main' :: IO ()
main' = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      (2 + 2) == 4 `shouldBe` True
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)
