#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

                           -- #!/bin/bash
import           Turtle
                           --
main = do                  --
    dir  <- pwd            -- DIR=$(pwd)
    time <- datefile dir   -- TIME=$(date -r $DIR)
    print time             -- echo $TIME
    print dir
