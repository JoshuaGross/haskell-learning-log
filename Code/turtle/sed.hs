#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as Text
import           Turtle

main = do
  stdout $ sed (fmap Text.reverse $ plus dot) (input "README.md") -- match everything; reverse
  stdout $ sed (digit *> return "!") (input "README.md") -- replace numbers with !
  stdout $ sed ("C" *> return "D") (input "README.md")   -- replace C with D
