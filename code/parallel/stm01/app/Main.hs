-- From:
-- https://en.wikibooks.org/wiki/Haskell/Concurrency
module Main where

import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import           Control.Monad.STM

oneSecond = 1000000
tenthSecond = div oneSecond 10

-- Write N times with delays in between
-- Must be killed explicitly
writerThread :: TChan Int -> Int -> IO ()
writerThread chan x = do
  atomically $ writeTChan chan x
  threadDelay tenthSecond
  writerThread chan (x+1)

-- read infinitely
-- will do once `main` ends or the writer stops writing (not sure which)
readerThread :: TChan Int -> IO ()
readerThread chan = do
  newInt <- atomically $ readTChan chan
  putStrLn $ "read new value: " ++ show newInt
  readerThread chan

main = do
  chan <- atomically $ newTChan
  rThread <- forkIO $ readerThread chan
  wThread <- forkIO $ writerThread chan 0
  threadDelay $ 5 * oneSecond
  --killThread rThread
  killThread wThread
