#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

import           Turtle

main = do
  putStr ">> "
  n <- readline
  print n
  putStr ">> "
  view $ liftIO readline
