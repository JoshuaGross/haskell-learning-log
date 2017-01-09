module Either where

lefts' :: [Either a b] -> [a]
lefts' = foldr (\a b -> leftToList a ++ b) []

rights' :: [Either a b] -> [b]
rights' = foldr (\a b -> rightToList a ++ b) []

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr (\a (b, c) -> (leftToList a ++ b, rightToList a ++ c)) ([], [])

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right x) = Just $ f x

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x)  = f x
either' _ f (Right x) = f x

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)

leftToList :: Either a b -> [a]
leftToList (Left a) = [a]
leftToList _ = []

rightToList :: Either a b -> [b]
rightToList (Right b) = [b]
rightToList _ = []
