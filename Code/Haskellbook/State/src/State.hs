module State where

newtype MyState s a = MyState { runMyState :: s -> (a, s) }

-- runMyState ((+1) <$> (MyState $ \s -> (0, s))) 0
instance Functor (MyState s) where
  fmap f' (MyState f) = MyState $ \s -> let (a, s') = f s in (f' a, s')

-- f (a -> b)
-- MyState s (a -> b) => runMyState: s -> ((a -> b), s)

-- runMyState ((+1) <$> (MyState $ \s -> (0, s))) 0
-- runMyState (pure 3) 0
-- runMyState (pure (+1) <*> pure 3) 0
instance Applicative (MyState s) where
  pure x = MyState $ \s -> (x, s)
  x <*> y = MyState $ \s -> let (a, s') = (runMyState y s) in
                            let (f, s'') = (runMyState x s') in
                            (f a, s'')

-- runMyState ((pure 1 :: MyState Int Int) >>= return) 0
-- runMyState ((pure 1 :: MyState Int Int) >>= return . (+1)) 0
-- runMyState ((pure 1 :: MyState Int Int) >>= \a -> MyState $ \s -> (s, a + s)) 0
-- runMyState ((pure 1 :: MyState Int Int) >>= \a -> MyState $ \s -> (s, a + s)) 1
-- runMyState ((pure 1 :: MyState Int Int) >>= \a -> MyState $ \s -> (s, a + s)) 2
instance Monad (MyState s) where
  return = pure
  --(>>=) :: MyState s a -> (a -> MyState s b) -> MyState s b
  v >>= f = MyState $ \s -> let (a, s')  = runMyState v s in
                            let (b, s'') = runMyState (f a) s' in
                            (b, s'')
