module Vehicles where

--data Price = Price Integer deriving (Eq, Show)
newtype Price = Price Integer deriving (Eq, Show)

class TooExpensive a where
  tooExpensive :: a -> Bool

instance TooExpensive Price where
  tooExpensive (Price n) = n > 15000

data Manufacturer =
    Mini
  | Mazda
  | Tata
  deriving (Eq, Show)

data Airline =
    PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle =
    Car Manufacturer Price
  | Plane Airline
  deriving (Eq, Show)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

areAllCars :: [Vehicle] -> Bool
areAllCars = all isCar

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m
getManu _ = undefined

myCar    = Car Mini (Price 14000)
urCar    = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge     = Plane PapuAir
