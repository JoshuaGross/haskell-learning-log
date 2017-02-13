{-# LANGUAGE OverloadedStrings #-}

module Fractions where

import           Control.Applicative
import           Data.Ratio          ((%))
import           Text.Trifecta

badFractions :: [String]
badFractions = ["1/0", "10"]

goodFractions :: [String]
goodFractions = ["1/2", "2/1"]

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  _ <- char '/'
  denominator <- decimal
  return (numerator % denominator)

virtuousParser :: Parser Rational
virtuousParser = do
  numerator <- decimal
  _ <- char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

mainFractions :: IO ()
mainFractions = do
  mapM_ (print . parseString virtuousParser mempty) goodFractions
  mapM_ (print . parseString virtuousParser mempty) badFractions
  print "foo"
