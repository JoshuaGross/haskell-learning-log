module Main where

import           Network.Wai.Handler.Warp
import           Server

main :: IO ()
main = run 8081 app1
