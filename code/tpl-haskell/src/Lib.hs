{-# LANGUAGE TemplateHaskell #-}

module Lib where

import           CustomShow
import           MakeClassyConstraints
import           MakeClassySubclassing

import           Control.Lens
import           Control.Lens.Internal.FieldTH

data MyData = MyData
    { foo :: String
    , bar :: Int
    }

listFields ''MyData

someFunc :: IO ()
someFunc = print $ MyData { foo = "bar", bar = 5 }

--
-- Test makeClassyConstraints
--
class HasSomeArbitaryClass a where
  doSomething :: a -> String

data TypeA = TypeA { _field1 :: Int } deriving Show
makeClassyConstraints ''TypeA [''Show, ''HasSomeArbitaryClass]
data TypeB = TypeB { _field2 :: Int } deriving Show
makeClassyConstraints ''TypeB [''Show]
data TypeC = TypeC { _fieldForA :: TypeA, _fieldForB :: TypeB } deriving Show
makeClassyConstraints ''TypeC [''HasTypeA, ''HasTypeB]

-- (HasTypeA a, HasTypeB a) => TypeC a

instance HasSomeArbitaryClass TypeA where
  doSomething = show

instance HasSomeArbitaryClass TypeB where
  doSomething = show

instance HasSomeArbitaryClass TypeC where
  doSomething = show

instance HasTypeA TypeC where
  typeA = fieldForA
instance HasTypeB TypeC where
  typeB = fieldForB

--
-- Test lens subclassing
--
data TypeX = TypeX { _x :: Int } deriving Show
makeClassyConstraints ''TypeX []
data TypeY = TypeY { _y :: Int } deriving Show
makeClassyConstraints ''TypeY []

data TypeZ' = TypeZ' { _z' :: Int } deriving Show
makeClassyAwesome ''TypeZ' [''TypeX, ''TypeY] [''Show] []

z' = TypeZ (TypeY 1) (TypeX 2) 3

-- Test lens 2nd-level subclassing...
data TypeZZXY' = TypeZZXY' { _zz' :: Int } deriving Show
makeClassyAwesome ''TypeZZXY' [''TypeZ] [''Show] []

zzxy' = TypeZZXY (TypeZ (TypeY 1) (TypeX 2) 3) 4

-- Test lens 3rd-level subclassing
data TypeZZXY2' = TypeZZXY2' { _zz2' :: Int } deriving Show
makeClassyAwesome ''TypeZZXY2' [''TypeZZXY] [''Show] []

zzxy2' = TypeZZXY2 (TypeZZXY (TypeZ (TypeY 2) (TypeX 3) 4) 5) 6

-- To view some TH in ghci:
-- Follow along with https://ocharles.org.uk/blog/guest-posts/2014-12-22-template-haskell.html
-- stack ghci --ghc-options -XTemplateHaskell
-- \> :m + Language.Haskell.TH
--
-- To view an expression in Oxford brackets:
-- \> runQ [| expr |]
-- or any expression that returns a `Q a`:
-- \> runQ (someExpr)

-- To view splicing expressions:
-- stack ghci --ghc-options -XTemplateHaskell --ghc-options -ddump-splices
-- \> $(primeQ ($(primeQ 0 23) !! 3) 167)

-- Test `reify` on a type:
-- \> $(stringE . show =<< reify ''Bool)
-- \> $(stringE . show =<< reify ''Int)
-- \> $(stringE . show =<< reify ''TypeZ)

-- play around with `Name`:
-- \> $(stringE . show =<< reify (Name (OccName "TypeZ") NameS))
