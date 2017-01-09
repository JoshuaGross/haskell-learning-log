#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}
import           Turtle

example = do
    f <- ls "/tmp"       -- this actually iterates over all the files
    liftIO $ print f
    liftIO $ print "foo"

main = sh example
