#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

                                    -- #!/bin/bash
{-# LANGUAGE OverloadedStrings #-}
                                    --
import           Turtle
                                    --
main = do                           --
    echo "Line 1"                   -- echo Line 1
    echo "Line 2"                   -- echo Line 2
    shell "echo foo" empty
    shell "touch foo.bar" empty
    d <- datefile "foo.bar"
    print d
