module Mem where

import           Data.Monoid

newtype Mem s a = Mem { runMem :: s -> (a,s) }

instance Monoid a => Monoid (Mem s a) where
  mempty  = Mem $ \s -> (mempty, s)
  mappend (Mem { runMem=f }) (Mem { runMem=g }) =
    Mem $
      \s ->
        let (a, t) = f s in
        let (b, u) = g t in
        (a <> b, u)
