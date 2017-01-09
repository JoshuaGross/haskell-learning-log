module Combinatorial where

import           Data.Monoid

-- get all combos of N lists of elements
comboOfN :: Monoid a => a -> [[a]] -> [a]
comboOfN sep (z:zs) = foldl ((joinCombinations .) . makeCombinations) z zs
  where
    makeCombinations x y = zip (take (length y) (repeat x)) y
    joinCombinations = concatMap $ \(xs, y) -> map (\x -> x <> sep <> y) xs
