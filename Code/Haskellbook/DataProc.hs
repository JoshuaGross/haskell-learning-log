import           Data.List
import           Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

utctime2 :: UTCTime
utctime2 = UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123)

utctime3 :: UTCTime
utctime3 = UTCTime (fromGregorian 2001 5 1) (secondsToDiffTime 35234)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

theDatabase2 :: [DatabaseItem]
theDatabase2 =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbNumber 9002
  , DbNumber 9003
  , DbString "Hello, world!"
  , DbString "sup y'all"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  , DbDate (UTCTime (fromGregorian 1921 5 2) (secondsToDiffTime 34123))
  , DbDate (UTCTime (fromGregorian 2001 5 3) (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr f []
  where f (DbDate x) b = x : b
        f _ b = b

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr f []
  where f (DbNumber x) b = x : b
        f _ b = b

-- unsafe because it can't deal with empty lists
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = last . sort . filterDbDate

-- safe cuz it can handle empty lists
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb ds = fromIntegral $ div t l
  where
    t :: Integer
    t = sum ns

    l :: Integer
    l = toInteger $ length ns

    ns :: [Integer]
    ns = filterDbNumber ds
