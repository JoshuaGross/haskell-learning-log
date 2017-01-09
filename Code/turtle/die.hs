#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

import           Turtle

main = sh $ do
  dir       <- using (mktempdir "/tmp" "turtle")
  (file, _) <- using (mktemp dir "turtle")
  liftIO (print file)
  liftIO (die "Urk!")
