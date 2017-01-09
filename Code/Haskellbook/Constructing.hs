module Constructing where

data Id a = MkId a deriving (Eq, Show)
data Product a b = Product a b deriving (Eq, Show)
data Sum a b = First a | Second b deriving (Eq, Show)

data RecordProduct a b =
  RecordProduct { pfirst  :: a
                , psecond :: b }
                deriving (Eq, Show)

-- if the counts could overflow,
-- then the farm can afford the
-- programmer time to convert
-- the system
newtype NumCow = NumCow Int deriving (Eq, Show)
newtype NumPig = NumPig Int deriving (Eq, Show)
--data Farmhouse = Farmhouse NumCow NumPig deriving (Eq, Show)
type Farmhouse = Product NumCow NumPig

-- Nesting Product further
newtype NumSheep = NumSheep Int deriving (Eq, Show)
--data BigFarmhouse = BigFarmhouse NumCow NumPig NumSheep deriving (Eq, Show)
type BigFarmhouse = Product NumCow (Product NumPig NumSheep)

-- Play with Sum
type Name = String
type Age = Int
type LovesMud = Bool
type PoundsOfWool = Int
data CowInfo = CowInfo Name Age deriving (Eq, Show)
data PigInfo = PigInfo Name Age LovesMud deriving (Eq, Show)
data SheepInfo = SheepInfo Name Age PoundsOfWool deriving (Eq, Show)
--data Animal = Cow CowInfo | Pig PigInfo | Sheep SheepInfo deriving (Eq, Show)
type Animal = Sum CowInfo (Sum PigInfo SheepInfo)

farmhouseA :: Farmhouse
farmhouseA = Product (NumCow 1) (NumPig 2)
