module MaybeT where

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance Applicative m => Applicative (MaybeT m) where
  pure = MaybeT . pure . Just
  -- f :: m (Maybe (a -> b))
  -- x :: m (Maybe a)
  (MaybeT f) <*> (MaybeT x) = MaybeT $ (<*>) <$> f <*> x

instance Monad m => Monad (MaybeT m) where
  return = pure
  (MaybeT x) >>= f =
    MaybeT $ do
      x' <- x
      case x' of
        Nothing -> return Nothing
        Just y  -> runMaybeT (f y)
