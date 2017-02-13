module Main where

import           Prelude     hiding (sin)

import           Foreign.C
import           Foreign.Ptr (Ptr, nullPtr)

-- pure function
foreign import ccall "sin" c_sin :: CDouble -> CDouble
sin :: Double -> Double
sin = realToFrac . c_sin . realToFrac

-- impure function
foreign import ccall "time" c_time :: Ptr a -> IO CTime
getTime :: IO CTime
getTime = c_time nullPtr

main :: IO ()
main = do
  putStr "Enter number > "
  print . sin =<< readLn
  print =<< getTime
