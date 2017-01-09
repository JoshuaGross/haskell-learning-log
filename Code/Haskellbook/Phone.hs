module Phone where

import           Control.Arrow
import           Data.Char
import           Data.List
import           Data.Map      (Map)
import qualified Data.Map      as Map

type Digit = Char
type Presses = Int

data PhoneStructure = Map Digit String

digits :: PhoneStructure
digits = Map.fromList $ ('#', ",."):map (first (head . show))
  [(1, "1"), (2, "abc2"), (3, "def3"), (4, "ghi4"), (5, "jkl5"),
   (6, "mno6"), (7, "pqrs7"), (8, "tuv8"), (9, "wxyz9"), (0, " 0")]

convo :: [String]
convo =
  ["Wanna play 20 questions",
   "Ya",
   "U 1st haha",
   "Lol ok. Have u ever tasted alcohol lol",
   "Lol ya",
   "Wow ur cool haha. Ur turn",
   "Ok. Do u think I am pretty Lol",
   "Lol ya",
   "Haha thanks just making sure rofl ur turn"]

reverseTaps :: PhoneStructure -> Char -> [(Digit, Presses)]
reverseTaps = undefined
