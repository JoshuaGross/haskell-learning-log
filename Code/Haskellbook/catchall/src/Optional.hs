module Optional where

import           Data.Monoid

data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mappend Nada Nada = mempty
  mappend Nada x = x
  mappend x Nada = x
  mappend (Only x) (Only y) = Only $ mappend x y
  mempty = Only $ mempty

example1 :: Optional (Sum Integer)
example1 = Only (Sum 1)

example2 :: Optional (Sum Integer)
example2 = (Only (Sum 1)) <> (Only (Sum 2))

example3 :: Optional (Product Integer)
example3 = (Only (Product 3)) <> (Only (Product 4))

example4 :: Optional (Sum Integer)
example4 = Nada <> Nada

example5 :: Optional (Product Integer)
example5 = Nada <> Nada

example6 :: Optional [Integer]
example6 = Nada <> Only [1]
