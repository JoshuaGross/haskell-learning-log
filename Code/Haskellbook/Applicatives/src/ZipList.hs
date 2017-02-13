module ZipList where

import           Data.Semigroup
import           ListAp
import           Test.QuickCheck          (Arbitrary, CoArbitrary, arbitrary,
                                           oneof)
import           Test.QuickCheck.Checkers (EqProp, eq, (=-=))

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Semigroup (ZipList' a) where
  (<>) (ZipList' a) (ZipList' b) = ZipList' (a <> b)

instance Monoid (ZipList' a) where
  mempty = ZipList' mempty
  mappend = (<>)

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' (fmap f xs)

instance Applicative ZipList' where
  pure = ZipList' . pure
  (ZipList' (Cons f fs)) <*> (ZipList' (Cons x xs)) = ZipList' (Cons (f x) Nil) <> (ZipList' fs <*> ZipList' xs)
  (ZipList' Nil) <*> _ = ZipList' Nil
  _ <*> (ZipList' Nil) = ZipList' Nil

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

infZipListApTest :: ZipList' Int
infZipListApTest = ZipList' fns <*> ZipList' (infList' 100)

infZipListApTestOk :: Bool
infZipListApTestOk = infZipListApTest == ZipList' (Cons 101 (Cons 202 Nil))
