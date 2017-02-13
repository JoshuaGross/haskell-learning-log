module Main where

import           Apl1
import           BadMonoid
import           Control.Applicative
import           Data.Monoid
import           ListAp
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

main :: IO ()
main = do
  quickBatch (monoid Twoo)
  quickBatch (applicative (undefined :: [(String, String, Int)]))
  quickBatch (monoid (ZipList [Sum 1 :: Sum Int]))
  quickBatch (monoid (Cons 'a' Nil))
  quickBatch (applicative (undefined :: [(List Int, List Int, Int)]))
