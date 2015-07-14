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
