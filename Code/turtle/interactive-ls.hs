#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

import           Turtle

main = do
  putStr ">> "
  n <- readline
  maybe (print "Invalid input") (view . ls . fromText) n
