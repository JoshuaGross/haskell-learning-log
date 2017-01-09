module Sum where

data Sum a b = First a | Second b deriving (Eq, Show)
data Twitter = Twitter deriving (Eq, Show)
data AskFm = AskFm deriving (Eq, Show)

socialNetwork :: Sum Twitter AskFm
socialNetwork = First Twitter
