-- Doing junk with associative arrays (basically)

-- import qualified Data.Map as Map
-- Map.fromList [("hello","there")] ^.at "hello"
-- >> Just "there"
-- Map.fromList [("hello","there")] & at "hello" ?~ "world"
-- >> fromList [("hello","world")]

-- When setting a field in a struct (?) before, we used this construction:
--  a & bar .~ 3
-- Now we are using ?~ instead. What's up?

λ> :t (.~)
(.~) :: ASetter s t a b -> b -> s -> t
λ> :t (?~)
(?~) :: ASetter s t a (Maybe b) -> b -> s -> t

