#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

import qualified Control.Foldl as Fold
import           Turtle

main = do
  view $ fold (ls "/tmp") Fold.length
  view $ fold (ls "/tmp") Fold.head
  view $ fold (ls "/tmp") Fold.list
