#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

import           Turtle

main = stdout $ grep ((star dot) <> "monads" <> (star dot)) $ input "README.md"
