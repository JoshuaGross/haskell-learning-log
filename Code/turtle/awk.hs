#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

import           Turtle

main = stdout (inproc "awk" ["{ print $1 }"] "123 456")
