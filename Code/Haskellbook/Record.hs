module Code.Haskellbook.Record where

import           Data.List

data OperatingSystem = GnuPlusLinux | OpenBSDPlusNevermindJustBSDStill | Mac | Windows deriving (Eq, Show, Enum)

data ProgrammingLanguage = Haskell | Agda | Idris | PureScript deriving (Eq, Show, Enum)

data Programmer = Programmer { os :: OperatingSystem , lang :: ProgrammingLanguage } deriving (Eq, Show)

nineToFive :: Programmer
nineToFive = Programmer { os = Mac, lang = Idris }

-- You can iterate all enum values by referencing the first one,
-- if the datatype is an instance of Enum
allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [GnuPlusLinux ..]

allProgrammingLanguages :: [ProgrammingLanguage]
allProgrammingLanguages = [Haskell ..]

allProgrammers :: [Programmer]
allProgrammers = nub $ foldr (\(a,b) c -> Programmer a b : c) [] (comboOfN allOperatingSystems allProgrammingLanguages)

-- get all combos of N lists of elements
-- Modified from Combinatorial.hs
comboOfN :: [a] -> [b] -> [(a, b)]
comboOfN = (joinCombinations .) . makeCombinations
  where
    makeCombinations x y = zip (replicate (length y) x) y
    joinCombinations = concatMap $ \(xs, y) -> map (flip (,) y) xs
