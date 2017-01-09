module Hutton where

data Expr =
    Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit x) = x
eval (Add x y) = (+) (eval x) (eval y)

print :: Expr -> String
print (Lit x) = show x
print (Add x y) = (Hutton.print x) ++ " + " ++ (Hutton.print y)
