module Standard where

import           Data.Monoid

-- TODO: replace with more general version of myAnd that uses `Foldable` instead of []
-- See: https://hackage.haskell.org/package/base-4.9.0.0/docs/src/Data.Foldable.html#
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (True : xs) = myAnd xs
myAnd (False : xs) = False

myAnd' :: [Bool] -> Bool
myAnd' = foldr (&&) True

myOr :: [Bool] -> Bool
myOr [] = False
myOr (True : xs) = True
myOr (False : xs) = myOr xs

myOr' :: [Bool] -> Bool
myOr' = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs)
  | x' == True = True
  | otherwise = myAny f xs
  where x' = f x

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f = foldr ((||) . f) False

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (x:xs)
  | match == True = True
  | otherwise = myElem e xs
  where match = e == x

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = myAny' (== x)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

myReverse' :: [a] -> [a]
myReverse' = foldl (flip (:)) mempty

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ (squish xs)

squish' :: [[a]] -> [a]
squish' = foldr (foldr (:)) mempty

-- Test:
-- > squish [[1,2,3], [4,5,6]]
-- [1,2,3,4,5,6]

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap = (squish .) . fmap

-- Test:
-- > squishMap (\x -> [1, x, 3]) [2]
-- [1,2,3]
-- > squishMap (\x -> "WO "++[x]++" HOO ") "123"
-- "WO 1 HOO WO 2 HOO WO 3 HOO "

-- Given an ordering function, an "extreme" ordering value (LT or GT),
-- and a list, select the extreme value (the min or max).
selectExtremeValue :: Ordering -> (a -> a -> Ordering) -> [a] -> a
selectExtremeValue _ _ [] = undefined
selectExtremeValue _ _ (x:[]) = x
selectExtremeValue o f (x:(y:ys))
  | f x y == o = selectExtremeValue o f (x:ys)
  | otherwise  = selectExtremeValue o f (y:ys)

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy = selectExtremeValue GT

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy = selectExtremeValue LT

myMaximum :: Ord a => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: Ord a => [a] -> a
myMinimum = myMinimumBy compare
