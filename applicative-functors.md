# Applicative functors!
## Preamble

This file follows my progress understanding the paper "Applicative programming with effects" by McBridge and Paterson.

```
> ghci --version
The Glorious Glasgow Haskell Compilation System, version 7.8.3
> ghci
> :set -XNoMonomorphismRestriction
```

## Sequencing commands, page 1
```
> :t sequence
sequence :: Monad m => [m a] -> m [a]
> :t sequence :: [IO a] -> IO [a]
sequence :: [IO a] -> IO [a] :: [IO a] -> IO [a]
> sequence [Just 1, Just 2]
Just [1,2]
```

And, because Haskell lists exhibit nondeterminism:  http://stackoverflow.com/questions/5299295/why-does-application-of-sequence-on-list-of-lists-lead-to-computation-of-its-c

```
> sequence [[1,2], [3,4]]
[[1,3],[1,4],[2,3],[2,4]]
```

is equivalent to:

```
> :{
do x <- [1,2]
   y <- [3,4]
   return [x,y]
:}
[[1,3],[1,4],[2,3],[2,4]]
```

And for clarity, switch the `x,y` in the return:

```
> :{
do x <- [1,2]
   y <- [3,4]
   return [y,x]
:}
[[3,1],[4,1],[3,2],[4,2]]
```

So, let us implement our own sequence as in the paper and iterate:

```
> :{
let sequence' :: Monad m => [m a] -> m [a]
    sequence' [] = return []
    sequence' (c:cs) = do
        x <- c
        xs <- sequence' cs
        return (x:xs)
:}
> sequence' [Just 1, Just 2]
Just [1,2]
```

Apparently the standard monad library provides a function, `ap`, which closely resembles this.

```
> :t ap
ap :: Monad m => m (a -> b) -> m a -> m b
> :{
let sequence' :: Monad m => [m a] -> m [a]
    sequence' [] = return []
    sequence' (x:xs) = return (:) `ap` x `ap` sequence' xs
:}
> sequence' [Just 1, Just 2]
> 
```

Or, removing the infix notation to make sure I understand what's happening. Perhaps this is an admission that I don't get Haskell's operator precedence yet.

```
> :{
let sequence' :: Monad m => [m a] -> m [a]
    sequence' [] = return []
    sequence' (x:xs) = return (:) `ap` x `ap` (sequence' xs)
:}
> sequence' [Just 1, Just 2]
Just [1,2]
> :{
let sequence' :: Monad m => [m a] -> m [a]
    sequence' [] = return []
    sequence' (x:xs) = (return (:)) `ap` x `ap` (sequence' xs)
:}
> sequence' [Just 1, Just 2]
Just [1,2]
> :{
let sequence' :: Monad m => [m a] -> m [a]
    sequence' [] = return []
    sequence' (x:xs) = (((return (:)) `ap`) x `ap` (sequence' xs))
:}
> sequence' [Just 1, Just 2]
Just [1,2]
> :{
let sequence' :: Monad m => [m a] -> m [a]
    sequence' [] = return []
    sequence' (x:xs) = (ap (return (:)) x `ap` (sequence' xs))
:}
> sequence' [Just 1, Just 2]
Just [1,2]
> :{
let sequence' :: Monad m => [m a] -> m [a]
    sequence' [] = return []
    sequence' (x:xs) = (ap (return (:)) x) `ap` (sequence' xs)
:}
> sequence' [Just 1, Just 2]
Just [1,2]
> :{
let sequence' :: Monad m => [m a] -> m [a]
    sequence' [] = return []
    sequence' (x:xs) = ap (ap (return (:)) x) (sequence' xs) 
:}
> sequence' [Just 1, Just 2]
Just [1,2]
> :{
let sequence' :: Monad m => [m a] -> m [a]
    sequence' [] = return []
    sequence' (x:xs) = ap (ap (return (:)) x) $ sequence' xs
:}
> sequence' [Just 1, Just 2]
Just [1,2]
```

To be sure, my version is way less readable. Neither form are particularly intuitive to me at the moment, so I'll leave this here for posterity.

## Matrix transpose, page 2

```
> :t zipWith
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
> :t repeat
repeat :: a -> [a]
> :t zipWith (:)
zipWith (:) :: [a] -> [[a]] -> [[a]]
```

Given `[a]` and `[[a]]`, we will get `[[a]]`. Or, given a row of a matrix and the rest of a matrix, we will get a complete matrix. In particular, each [a] will be prepended to another list, so
even with `[a]:[]` (`[a]` prepended to an empty list) we would always have `[[a]]`.

```
> zipWith (:) [1,2,3] [[4,5,6],[7,8,9]]
[[1,4,5,6],[2,7,8,9]]
```

Let's finish the definition of transpose, and test it:

```
> :{
let transpose :: [[a]] -> [[a]]
    transpose [] = repeat []
    transpose (xs:xss) = zipWith (:) xs (transpose xss)
:}
> let x = [[1,2,3],[4,5,6],[7,8,9]]
> transpose x
[[1,4,7],[2,5,8],[3,6,9]]
> transpose (transpose x) == x
True
```

Success! Even though recursion still hurts my poor little brain.

The paper now mentions a result from the Fridlender, Indrika (2000) paper which I believe is here: http://www.brics.dk/RS/01/10/BRICS-RS-01-10.pdf

In particular, we are given two functions. One of them, `repeat`, is standard. The other, `zapp`, we must define.

```
> :t repeat
repeat :: a -> [a]
```

For example, `repeat 3` gives us a list of length infinity, composed only of element `3` repeated.

```
> :{
let zapp :: [a -> b] -> [a] -> [b]
    zapp (f:fs) (x:xs) = (f x) : (zapp fs xs)
    zapp _      _      = []
:}
```

And then we can redefine transposition as:

```
> :{
let transpose :: [[a]] -> [[a]]
    transpose [] = repeat []
    transpose (xs:xss) = repeat (:) `zapp` xs `zapp` transpose xss
:}
> transpose x
[[1,4,7],[2,5,8],[3,6,9]]
> :{
let transpose :: [[a]] -> [[a]]
    transpose [] = repeat []
    transpose (xs:xss) = zapp (zapp (repeat (:)) xs) $ transpose xss
:}
> transpose x
[[1,4,7],[2,5,8],[3,6,9]]
> :t zapp (repeat (:))
zapp (repeat (:)) :: [a] -> [[a] -> [a]]
```

Again, I show both the compact infix form and the non-infix form.

Even though we use infinite lists, `zapp` has a very conservative base-case so once we've exhausted `xs:xss`, we're done.

## Evaluating environments, page 3
We are given the sketch below (the following does not type-check yet):

```
> :{
data Exp v = Var v
           | Val Int
           | Add (Exp v) (Exp v)
eval :: Exp v -> Env v -> Int
eval (Var x) y = fetch x y
eval (Val i) y = i
eval (Add p q) y = (eval p y) + (eval q y)
:}
```

Unfortunately it is not complete and we are left to implement `Env` and `fetch` on our own. Let's have at it!

Let's implement a type that's essentially just a list of (name,value) pairs:

```
> newtype Env a b = Env { showEnv :: [(a,b)]}
> showEnv $ Env [("a",1)]
[("a",1)]
```

This should be enough for us; we probably don't need a monad since we don't particularly need to modify the environment.

```
> let fetch x (Env (y:ys)) = let (name,val) = y in (if x == name then val else (fetch x (Env ys)))
> :t fetch
fetch :: Eq a => a -> Env a t -> t
> fetch "a" (Env [("a",1)])
1
> fetch "b" (Env [("a",1)])
*** Exception: <interactive>:945:5-96: Non-exhaustive patterns in function fetch
```

Our system sort of works, but it's not very safe. We'll just keep this in mind for now and happily move along. We will also need to modify the original
example slightly, as `Env` now takes two arguments: the first argument (the "name") becomes `v`, and we know we want the "value" to always be `Int`. This
also requires constraining `v` to be part of the `Eq` typeclass, something that `fetch` needs.

```
> :{
data Exp v = Var v
           | Val Int
           | Add (Exp v) (Exp v)
eval :: Eq v => Exp v -> Env v Int -> Int
eval (Var x) y = fetch x y
eval (Val i) _ = i
eval (Add p q) y = (eval p y) + (eval q y)
:}
> eval (Val 1) (Env [])
1
> eval (Var "a") (Env [("a", 2)])
2
> eval (Add (Val 1) (Var "a")) (Env [("a", 2)])
3
```

Sweet.

Although the `S` and `K` combinators are just `ap` and `return` in `Monad` (respectively), we will implement them and use them explicitly.

```
> let kcomb x y = x
> let scomb ef es y = (ef y) (es y)
> :t kcomb
kcomb :: t1 -> t -> t1
> :t scomb
scomb :: (t2 -> t1 -> t) -> (t2 -> t1) -> t2 -> t
```

Though the type variables `tX` aren't as nice, these correspond directly to the definitions of `S` and `K` in the paper.

```
> :{
data Exp v = Var v
           | Val Int
           | Add (Exp v) (Exp v)
eval :: Eq v => Exp v -> Env v Int -> Int
eval (Var x) = fetch x
eval (Val i) = kcomb i
eval (Add p q) = kcomb (+) `scomb` eval p `scomb` eval q
:}
> eval (Val 1) (Env [])
1
> eval (Var "a") (Env [("a", 2)])
2
> eval (Add (Val 1) (Var "a")) (Env [("a", 2)])
3
```

Phew! That is equivalent to using `return` and `ap` from Monad:

```
> :{
data Exp v = Var v
           | Val Int
           | Add (Exp v) (Exp v)
eval :: Eq v => Exp v -> Env v Int -> Int
eval (Var x) = fetch x
eval (Val i) = return i
eval (Add p q) = return (+) `ap` eval p `ap` eval q
:}
> eval (Val 1) (Env [])
1
> eval (Var "a") (Env [("a", 2)])
2
> eval (Add (Val 1) (Var "a")) (Env [("a", 2)])
3
```

Showing that `return = kcomb` and `ap = scomb`:

```
> :t return
return :: Monad m => a -> m a
> :t kcomb
kcomb :: t1 -> t -> t1
> :t ap
ap :: Monad m => m (a -> b) -> m a -> m b
> :t scomb
scomb :: (t2 -> t1 -> t) -> (t2 -> t1) -> t2 -> t
```

This also shows that you don't always need monads to do interesting, monad-y things (which is the point of this paper).

For completeness, let's revise our `Env` to be type-safe using `Maybe`. We will use pattern-matching with an additional function for `add` to avoid turning `Env` into a monad.

```
> let fetch x (Env (y:ys)) = let (name,val) = y in (if x == name then (Just val) else (if ys == [] then Nothing else (fetch x (Env ys))))
> :t fetch
fetch :: (Eq a1, Eq a) => a -> Env a a1 -> Maybe a1
> :{
data Exp v = Var v
           | Val Int
           | Add (Exp v) (Exp v)
add :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
add f Nothing _ = Nothing
add f _ Nothing = Nothing
add f (Just x) (Just y) = Just $ f x y
eval :: Eq v => Exp v -> Env v Int -> Maybe Int
eval (Var x) = fetch x
eval (Val i) = return $ Just i
eval (Add p q) = ap (ap (return (add (+))) (eval p)) (eval q)
:}
> eval (Val 1) (Env [])
Just 1
> eval (Var "a") (Env [("a", 2)])
Just 2
> eval (Add (Val 1) (Var "a")) (Env [("a", 2)])
Just 3
> eval (Add (Val 1) (Var "b")) (Env [("a", 2)])
Nothing
```

Wonderful! - And definitely something we should use applicative functors with explicitly; but, that is the point of this paper. We will return to this later.

## The Applicative class: page 3
