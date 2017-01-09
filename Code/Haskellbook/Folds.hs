module Folds where

-- From Haskellbook p350, adapted from examples in the Haskell
-- IRC channel.
--
-- > let xs = map show [1..5]
-- > foldr (\x y -> concat ["(", x, "+", y, ")"]) "0" xs
-- "(1+(2+(3+(4+(5+0)))))"
-- > foldl (\x y -> concat ["(", x, "+", y, ")"]) "0" xs
-- "(((((0+1)+2)+3)+4)+5)"


-- foldl, foldr never forces evaluation of the spine!
-- > foldr (\_ _ -> 0) 0 [1, 2, 3, undefined]
-- 0
-- > foldr (\_ _ -> 0) 0 [undefined]
-- 0
-- > foldl (\_ _ -> 0) 0 [1, 2, 3, undefined]
-- 0
-- > foldl (\_ _ -> 0) 0 [undefined]
-- 0

-- fold compared to scan:
-- > foldr (+) 0 [1..5]
-- 15
-- > foldl (+) 0 [1..5]
-- 15
-- > scanr (+) 0 [1..5]
-- [15,14,12,9,5,0]
-- > scanl (+) 0 [1..5]
-- [0,1,3,6,10,15]

-- Identities:
-- foldr = head . scanr
-- foldl = last . scanl
--
-- We cannot necessarily reason about other relationships between these.
