{-# LANGUAGE TupleSections #-}
module StateT where

import           Control.Arrow (first)

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance Functor m => Functor (StateT s m) where
  fmap f (StateT smas) = StateT $ fmap (first f) . smas

instance Applicative m => Applicative (StateT s m) where
  pure a = StateT $ pure . (a,)
  -- smfabs :: s -> m (a -> b, s)
  -- smas :: s -> m (a, s)
  --(StateT smfabs) <*> (StateT smas) = StateT $ \s -> let (fsmfabs s)
