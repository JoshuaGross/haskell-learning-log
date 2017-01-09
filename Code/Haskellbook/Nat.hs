module Nat where

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + (natToInteger n)

integerToNat :: Integer -> Maybe Nat
integerToNat 0 = Just Zero
integerToNat n
  | n < 0 = Nothing
  | n > 0 = maybe Nothing (Just . Succ) (integerToNat (n - 1))
