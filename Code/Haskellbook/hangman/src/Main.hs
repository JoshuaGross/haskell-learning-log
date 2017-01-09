module Main where

import           Control.Monad (forever)
import           Data.Char     (toLower)
import           Data.List     (intersperse)
import           Data.Maybe    (isJust)
import           System.Exit   (exitSuccess)
import           System.Random (randomRIO)

type WordList = [String]

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (fmap (const Nothing) s) ""

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _) = flip elem s

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ s) = flip elem s

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledIn s) c =
  Puzzle word newFilledIn (c:s)
  where zipper guessed wordChar guessChar =
          if wordChar == guessed
          then Just wordChar
          else guessChar
        newFilledIn = zipWith (zipper c) word filledIn

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that. Try again."
      return puzzle
    (True, _) -> do
      putStrLn "You guessed correctly."
      return $ fillInCharacter puzzle guess
    _ -> do
      putStrLn "That character is not in the word. Try again."
      return $ fillInCharacter puzzle guess

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return (lines dict)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  if (length guessed) > 7
  then do
    putStrLn "You lose! You're a big losey loser."
    putStrLn $ "The word was: " ++ wordToGuess
    exitSuccess
  else
    return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledIn _) =
  if all isJust filledIn
  then do
    putStrLn "You win!"
    exitSuccess
  else return ()

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  aw <- allWords
  return $ filter gameLength aw
  where gameLength w =
          let l = length (w :: String)
          in l >= minWordLength && l <= maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do
  i <- randomRIO (0, length wl - 1)
  return $ wl !! i

runGame :: Puzzle -> IO ()
runGame p = forever $ do
  gameOver p
  gameWin p
  putStrLn $ "Puzzle: " ++ show p
  putStr "Guess a letter > "
  guess <- getLine
  case guess of
    [c] -> handleGuess p c >>= runGame
    _   -> putStrLn "Your guess must be a single letter."

main :: IO ()
main = do
  wl <- gameWords
  word <- randomWord wl
  runGame (freshPuzzle $ fmap toLower word)
