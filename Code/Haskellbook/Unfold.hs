module Unfold where

-- take 10 $ myIterate (+1) 0
myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = myUnfoldr' x'
  where x' = f x
        myUnfoldr' Nothing  = []
        myUnfoldr' (Just (y, z)) = y : myUnfoldr f z

-- take 10 $ myIterate (+1) 0
betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\x -> let x' = f x in Just (x', x'))
