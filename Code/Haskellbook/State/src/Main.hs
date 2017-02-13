module Main where

import           Control.Applicative       (liftA3)
import           Control.Monad             (replicateM)
import           Control.Monad.Trans.State
import           System.Random

data Die = DieOne | DieTwo | DieThree | DieFour | DieFive | DieSix deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1,6)
  return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1,6))

rollDieThreeTimes :: State StdGen (Die, Die, Die)
rollDieThreeTimes = liftA3 (,,) rollDie' rollDie' rollDie'

-- no good!
--infiniteDie :: State StdGen [Die]
--infiniteDie = repeat <$> rollDie'

-- v good!
-- evalState (nDie 10) (mkStdGen 0)
nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie'

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty = rollsToGetN 20

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN = (fst .) . rollsCountLogged

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go [] 0 0 g
  where go :: [Die] -> Int -> Int -> StdGen -> (Int, [Die])
        go ds sum count gen
          | sum >= n = (count, ds)
          | otherwise = let (die, nextGen) = randomR (1, 6) gen
                        in go (intToDie die : ds) (sum + die) (count + 1) nextGen

main :: IO ()
main = do
  print "foo"
