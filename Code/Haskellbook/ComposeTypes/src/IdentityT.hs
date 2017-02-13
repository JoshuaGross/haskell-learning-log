module IdentityT where

newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

newtype IdentityT f a = IdentityT { runIdentityT :: f a } deriving (Eq, Show)

instance Functor m => Functor (IdentityT m) where
  fmap f (IdentityT x) = IdentityT (fmap f x)

instance Applicative m => Applicative (IdentityT m) where
  pure = IdentityT . pure
  (IdentityT f) <*> (IdentityT x) = IdentityT (f <*> x)

-- IdentityT [1, 2, 3] >>= (return . (+1))
instance Monad m => Monad (IdentityT m) where
  return = pure
  (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f
