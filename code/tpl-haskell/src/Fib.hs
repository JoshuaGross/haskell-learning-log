{-# LANGUAGE TemplateHaskell #-}

module Fib where

import           Language.Haskell.TH

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibQ :: Int -> Q Exp
fibQ n = [| fibs !! n |]
