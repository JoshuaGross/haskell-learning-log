module EitherT where

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance (Functor m) => Functor (EitherT e m) where
  fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea

instance Applicative m => Applicative (EitherT e m) where
  pure = EitherT . pure . Right
  (EitherT mab) <*> (EitherT ma) = EitherT $ (<*>) <$> mab <*> ma

instance Monad m => Monad (EitherT e m) where
  return = pure
  (EitherT x) >>= f =
    EitherT $ do
      x' <- x
      case x' of
        Left y  -> return $ Left y
        Right y -> runEitherT $ f y

