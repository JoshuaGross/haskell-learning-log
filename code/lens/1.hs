{-# LANGUAGE TemplateHaskell #-}
import Control.Lens

data Foo a = Foo { _b :: Int, _c :: Int, _d :: a } deriving (Show)
makeLenses ''Foo

a = Foo {_b = 1, _c = 2, _d = 3}
-- a ^. b
-- a ^. c
-- a ^. d
-- a & b .~ 5 -- set b=5
