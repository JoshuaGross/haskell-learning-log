# Maybe Monad
I've heard it's good practice to attempt to implement your own Maybe monad, so here goes.

Everything in this file can be run (in order) with `ghci`. I was using:

```
> ghci --version
The Glorious Glasgow Haskell Compilation System, version 7.8.3
> ghci
> :set -XNoMonomorphismRestriction
```

```
> :{
data Maybe' a = Nada | Some a
instance Monad Maybe' where
  return x = Some x
  Some x >>= f = f x
  Nada >>= f = Nada
:}
```

This type checks and we can construct and test `Maybe'` values:

```
> :t Some 4
Some 4 :: Num a => Maybe' a
> :t Nada
Nada :: Maybe' a
```

But (unlike in the `monads.md`) playground we don't have a `runX` function. How do we bind functions in the `Maybe'` monad?

First, let's make it possible for us to `Show` `Maybe'`:

```
> :{
instance Show a => Show (Maybe' a) where
  showsPrec _ Nada = shows "Nada"
  showsPrec _ (Some x) = shows $ "Some " ++ (show x)
:}
```

Now we can print our values using the IO monad which will make debugging easier:

```
> Some 4
"Some 4"
> Nada
"Nada"
```

Then, let's define a function "increment" that will take a `Maybe Int` and increment it:

```
> let incr x = Some (x+1)
> (Some 4) >>= incr
"Some 5"
> Nada >>= incr
"Nada"
```

Significantly easier than I expected!
