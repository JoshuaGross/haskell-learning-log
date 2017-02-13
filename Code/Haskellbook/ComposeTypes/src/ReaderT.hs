module ReaderT where

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ fmap f . rma

instance Applicative m => Applicative (ReaderT r m) where
  pure a = ReaderT $ \_ -> pure a
  -- fab :: r -> m (a -> b)
  -- a :: r -> m a
  -- f <*> x :: ReaderT r m b
  (ReaderT fmab) <*> (ReaderT rma) = ReaderT $ (<*>) <$> fmab <*> rma

instance Monad m => Monad (ReaderT r m) where
  return = pure

  (ReaderT rma) >>= f = ReaderT $ \r -> (rma r >>= (\a -> runReaderT (f a) r))
