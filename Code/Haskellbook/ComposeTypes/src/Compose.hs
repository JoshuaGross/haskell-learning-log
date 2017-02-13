module Compose where

-- (.)     :: (b -> c) -> (a -> b) -> a -> c
-- Compose :: (* -> *) -> (* -> *) -> * -> *
newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show)

-- (fmap . fmap) (+1) (Compose [Just (Compose $ Just [1])])
instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

-- (Compose (Just [(+1)])) <*> (Compose (Just [5]))
instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure = Compose . (pure . pure)
  (Compose f) <*> (Compose x) = Compose $ ((<*>) <$> f) <*> x
