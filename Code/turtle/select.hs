#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

                                    -- #!/bin/bash
{-# LANGUAGE OverloadedStrings #-}
                                    --
import           Turtle
                                    -- Permutations! What?!
example = do                        --
    x <- select [1, 2]              -- for x in 1 2; do
    y <- select [3, 4]              --     for y in 3 4; do
    liftIO (print (x, y))           --         echo \(${x},${y}\);
                                    --     done;
main = sh example                   -- done
