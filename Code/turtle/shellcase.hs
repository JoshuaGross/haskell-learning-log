#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

import           Turtle

main = do
    let cmd = "false"
    x <- shell cmd empty
    case x of
        ExitSuccess   -> return ()
        ExitFailure n -> die (cmd <> " failed with exit code: " <> repr n)
