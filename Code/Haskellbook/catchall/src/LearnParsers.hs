module LearnParsers where

import           Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

two :: Parser Char
two = char '2'

one' :: Parser Char
one' = one >> stop

oneEof :: Parser ()
oneEof = one >> eof

oneTwo :: Parser Char
oneTwo = one >> two

oneTwo' :: Parser ()
oneTwo' = oneTwo >> stop

oneTwoEof :: Parser ()
oneTwoEof = oneTwo >> eof

testParse :: Show a => Parser a -> IO ()
testParse p = print $ parseString p mempty "123"

pNL :: String -> IO ()
pNL s = putStrLn ('\n' : s)

learnParsersMain :: IO ()
learnParsersMain = do
  pNL "stop:"
  testParse (stop :: Parser Char)
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneEof:"
  testParse oneEof
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
  pNL "oneTwoEof:"
  testParse oneTwo'
