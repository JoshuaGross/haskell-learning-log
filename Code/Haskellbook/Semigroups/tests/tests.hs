module Main where

import           AccumulateBoth
import           AccumulateRight
import           BoolConj
import           BoolDisj
import           Combine
import           Four
import           Identity
import           Mem
import           Or
import           Pair
import           Three
import           Trivial
import           Two
import           Validation

import qualified Data.Functor             as F
import qualified Data.Monoid              as M
import           Data.Semigroup           (Semigroup, Sum (Sum), (<>))

import           Test.QuickCheck
import qualified Test.QuickCheck.Checkers as QCCH
import qualified Test.QuickCheck.Classes  as QCCL
import           Test.QuickCheck.Function

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, M.Monoid m) => m -> Bool
monoidLeftIdentity a = (a M.<> mempty) == a

monoidRightIdentity :: (Eq m, M.Monoid m) => m -> Bool
monoidRightIdentity a = (mempty M.<> a) == a

functorComposeCheck :: (Eq (f c), F.Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorComposeCheck x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

boolConjTest :: BoolConj -> BoolConj -> BoolConj -> Bool
boolConjTest a@(BoolConj a') b@(BoolConj b') c@(BoolConj c') =
  (a <> b) == BoolConj (a' && b') &&
  (b <> c) == BoolConj (b' && c') &&
  (a <> c) == BoolConj (a' && c')

type S = String   -- 1
type T = Trivial  -- 1
type I = Identity -- n
type IS = I S     -- 1
type ISX = I (I (I (I (I (I (I (I (I (I S))))))))) -- 1
type ITSX = I (Two (Two IS IS) (Two IS IS)) -- 4
type T' = Three (Two ITSX ITSX) ITSX ISX  -- 13
type F' = Four (Two ITSX ITSX) ITSX ISX T' -- 26
type FX  = Four F' T' F' T'      -- 78
type FXX = Four FX FX FX FX      -- 312
type FXXX = Four FXX FXX FXX FXX -- 1248
type FXXXX = Four FXXX FXXX FXXX FXXX -- 4992
type ORX = (Or IS IS)
type ORXX = (Or ITSX ORX)
type ORXXX = (Or ORXX FX)

type SomeAssoc a = a -> a -> a -> Bool

type SomeMonoidIdentity a = a -> Bool

type FunId x = Fun x x
type SomeFunctorComp a b = a b -> FunId b -> FunId b -> Bool

genISX :: Gen ISX
genISX = do
  a <- arbitrary
  return $ Identity (Identity (Identity (Identity (Identity (Identity (Identity (Identity (Identity (Identity a)))))))))

genITSX :: Gen ITSX
genITSX = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return $ Identity (Two (Two a b) (Two c d))

f :: Integer -> Sum Integer
f n = Sum (n + 1)

g :: Integer -> Sum Integer
g n = Sum (n - 1)

f' :: Combine Integer (Sum Integer)
f' = Combine f

g' :: Combine Integer (Sum Integer)
g' = Combine g

memf' :: Mem Integer String
memf' = Mem $ \s -> ("hi", s + 1)

main :: IO ()
main = do
  print "Test Combine:"
  print $ unCombine (f' <> g') (0 :: Integer)
  print $ unCombine (f' <> g') (1 :: Integer)
  print $ unCombine (f' <> g') (2 :: Integer)
  print $ unCombine (g' <> f') (0 :: Integer)
  print $ unCombine (g' <> f') (1 :: Integer)
  print $ unCombine (g' <> f') (2 :: Integer)
  print $ unCombine (f' <> f') (0 :: Integer)
  print $ unCombine (f' <> f') (1 :: Integer)
  print $ unCombine (f' <> f') (2 :: Integer)
  print $ unCombine (g' <> g') (0 :: Integer)
  print $ unCombine (g' <> g') (1 :: Integer)
  print $ unCombine (g' <> g') (2 :: Integer)
  print $ (unCombine (mappend f' mempty) $ 1)
  print "Test Mem:"
  print $ runMem (memf' M.<> M.mempty) 0
  print $ runMem (M.mempty M.<> memf') 0
  print $ (runMem M.mempty 0 :: (String, Int))
  print $ runMem (memf' M.<> M.mempty) 0 == runMem memf' 0
  print $ runMem (M.mempty M.<> memf') 0 == runMem memf' 0
  print "Semigroup QuickChecks:"
  quickCheck (semigroupAssoc :: SomeAssoc T)
  quickCheck (semigroupAssoc :: SomeAssoc IS)
  quickCheck (semigroupAssoc :: SomeAssoc (Two IS T))
  quickCheck (semigroupAssoc :: SomeAssoc (Two ISX ISX))
  quickCheck (semigroupAssoc :: SomeAssoc (Two ITSX ITSX))
  quickCheck (semigroupAssoc :: SomeAssoc T')
  quickCheck (semigroupAssoc :: SomeAssoc F')
  quickCheck (semigroupAssoc :: SomeAssoc FX)
  quickCheck (semigroupAssoc :: SomeAssoc BoolConj)
  quickCheck (semigroupAssoc :: SomeAssoc BoolDisj)
  quickCheck boolConjTest
  quickCheck (semigroupAssoc :: SomeAssoc (Or Int String))
  quickCheck (semigroupAssoc :: SomeAssoc ORX)
  quickCheck (semigroupAssoc :: SomeAssoc ORXX)
  quickCheck (semigroupAssoc :: SomeAssoc (Validation String String))
  quickCheck (semigroupAssoc :: SomeAssoc (Validation (Or IS String) (Or String IS)))
  quickCheck (semigroupAssoc :: SomeAssoc (AccumulateRight ORX ORXX))
  quickCheck (semigroupAssoc :: SomeAssoc (AccumulateRight ORXXX ORXXX))
  quickCheck (semigroupAssoc :: SomeAssoc (AccumulateBoth ORX ORXX))
  quickCheck (semigroupAssoc :: SomeAssoc (AccumulateBoth ORXXX ORXXX))
  print "Monoid QuickChecks:"
  quickCheck (monoidLeftIdentity :: SomeMonoidIdentity T)
  quickCheck (monoidRightIdentity :: SomeMonoidIdentity T)
  quickCheck (monoidLeftIdentity :: SomeMonoidIdentity IS)
  quickCheck (monoidRightIdentity :: SomeMonoidIdentity IS)
  quickCheck (monoidLeftIdentity :: SomeMonoidIdentity (Two IS T))
  quickCheck (monoidRightIdentity :: SomeMonoidIdentity (Two IS T))
  quickCheck (monoidLeftIdentity :: SomeMonoidIdentity T')
  quickCheck (monoidRightIdentity :: SomeMonoidIdentity T')
  quickCheck (monoidLeftIdentity :: SomeMonoidIdentity F')
  quickCheck (monoidRightIdentity :: SomeMonoidIdentity F')
  quickCheck (monoidLeftIdentity :: SomeMonoidIdentity BoolConj)
  quickCheck (monoidRightIdentity :: SomeMonoidIdentity BoolConj)
  quickCheck (monoidLeftIdentity :: SomeMonoidIdentity BoolDisj)
  quickCheck (monoidRightIdentity :: SomeMonoidIdentity BoolDisj)
  print "Functor QuickChecks:"
  quickCheck (functorComposeCheck :: SomeFunctorComp [] Int)
  quickCheck (functorComposeCheck :: SomeFunctorComp [] Char)
  --quickCheck (functorComposeCheck :: SomeFunctorComp [] String) -- slow
  quickCheck (functorComposeCheck :: SomeFunctorComp Identity Int)
  quickCheck (functorComposeCheck :: SomeFunctorComp Identity Char)
  --quickCheck (functorComposeCheck :: SomeFunctorComp Identity String) -- slow
  quickCheck (functorComposeCheck :: SomeFunctorComp Pair Int)
  quickCheck (functorComposeCheck :: SomeFunctorComp Pair Char)
  quickCheck (functorComposeCheck :: SomeFunctorComp (Two Int) Int)
  quickCheck (functorComposeCheck :: SomeFunctorComp (Two Int) Char)
  quickCheck (functorComposeCheck :: SomeFunctorComp (Three Int Int) Int)
  quickCheck (functorComposeCheck :: SomeFunctorComp (Three Int Int) Char)
  quickCheck (functorComposeCheck :: SomeFunctorComp (Four Int Int Int) Int)
  quickCheck (functorComposeCheck :: SomeFunctorComp (Four Int Int Int) Char)
  print "Applicative QuickChecks:"
  QCCH.quickBatch (QCCL.applicative (undefined :: [(Identity Int, Identity Int, Int)]))
  QCCH.quickBatch (QCCL.applicative (undefined :: [(Pair Int, Pair Int, Int)]))
  QCCH.quickBatch (QCCL.applicative (undefined :: [(Two Int Int, Two Int Int, Int)]))
  QCCH.quickBatch (QCCL.applicative (undefined :: [(Three Int Int Int, Three Int Int Int, Int)]))
  QCCH.quickBatch (QCCL.applicative (undefined :: [(Four Int Int Int Int, Four Int Int Int Int, Int)]))
