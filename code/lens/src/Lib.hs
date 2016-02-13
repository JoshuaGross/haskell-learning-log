{-# LANGUAGE TemplateHaskell #-}

module Lib
    ( someFunc
    ) where

import           Control.Lens

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Foo a = Foo { _b :: Int, _c :: Int, _d :: a } deriving (Show)
makeLenses ''Foo

a = Foo {_b = 1, _c = 2, _d = 3}
-- a ^. b
-- a ^. c
-- a ^. d
-- a & b .~ 5 -- set b=5

data TypeA = TypeA { _field1 :: Int } deriving Show
makeClassy ''TypeA

data TypeB = TypeB { _field2 :: Int } deriving Show
makeClassy ''TypeB

data TypeC = TypeC { _fieldForA :: TypeA, _fieldForB :: TypeB } deriving Show
makeClassy ''TypeC

instance HasTypeA TypeC where
  typeA = fieldForA
instance HasTypeB TypeC where
  typeB = fieldForB

c' = TypeC (TypeA 1) (TypeB 2)

-- c' ^. field1
-- c' ^. field2

data TypeD = TypeD { _fieldForC :: TypeC } deriving Show
makeClassy ''TypeD
instance HasTypeC TypeD where
  typeC = fieldForC
instance HasTypeA TypeD where
  typeA = fieldForC . typeA
instance HasTypeB TypeD where
  typeB = fieldForC . typeB

d' = TypeD (TypeC (TypeA 1) (TypeB 2))

-- d' ^. field1
-- d' ^. field2

-- TODO: write some macros to reduce this BS boilerplate!
