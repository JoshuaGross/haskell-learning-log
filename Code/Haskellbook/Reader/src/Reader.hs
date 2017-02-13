{-# LANGUAGE InstanceSigs #-}
module Reader where

import           Data.Char

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader r) = Reader $ f . r

--
instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure = Reader . const

  -- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
  -- (<*>) :: Applicative f => Reader r (a -> b) -> Reader r a -> Reader r b
  -- fmap  :: Functor f =>              (a -> b) -> f a        -> f b
  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader (rab <*> ra)
  -- outer `Reader x` :: Reader r b = r -> b
  -- `rab` :: r (a -> b) = r -> a -> b
  -- `ra` :: r a = r -> a

instance Monad (Reader r) where
  return = pure

  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  -- ra :: (r -> a)
  -- arb :: a -> Reader r b
  -- r :: r
  (Reader ra) >>= arb = Reader $ \r -> runReader (arb (ra r)) r

ask :: Reader a a
ask = Reader id

plusSomething :: Num a => Reader a (a -> a)
plusSomething = Reader (+)

multSomething :: Num a => Reader a (a -> a)
multSomething = Reader (*)

chrSomething :: Reader Int Char
chrSomething = Reader chr

chrSomething' :: Reader Int (Int -> Char)
chrSomething' = Reader ((chr .) . (+))

chrSomething'' :: Reader Int (Int -> Char)
chrSomething'' = Reader ((chr .) . (+))

-- runReader ask 1
-- runReader (plusSomething <*> ask) 1
-- runReader (multSomething <*> ask) 1
-- runReader (chrSomething' <*> ask) 1
-- runReader (chrSomething' <*> ask) 22
-- runReader chrSomething 44

