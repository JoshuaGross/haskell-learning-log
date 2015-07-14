# Monads!
## Preamble
In my attempt to learn Haskell, become comfortable with monads, and functional programming concepts in general,
I am following along with the blog post, "You Could Have Invented Monads! (And Maybe You Already Have.)"
http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html

Everything in this file can be run (in order) with `ghci`. I was using:

```
> ghci --version
The Glorious Glasgow Haskell Compilation System, version 7.8.3
> ghci
> :set -XNoMonomorphismRestriction
```

## Part One: debugging

In the first example, we have a set of pure functions that we want to debug. Let's say we have two functions that
manipulate floats:

```
> let f = \x -> x+5 :: Float
> let g = \x -> x/2 :: Float
> :t f
   f :: Float -> Float
> :t g
   g :: Float -> Float
> f (g 2)
   6.0
```

So far, so good. Next, the article asks how we can make `f` and `g` "effectful". Say we want to debug what's happening
inside of `f` and `g`. In most languages we would just issue a print statement or perhaps modify some global variable
with debugging information. In a pure environment, we can't do that, so our only choice is to have `f` and `g` return
strings:

```
> let f' = \x -> (x+5, "Added " ++ (show x) ++ " and " ++ (show 5)) :: (Float,String)
> let g' = \x -> (x/2, "Divided " ++ (show x) ++ " by " ++ (show 2)) :: (Float,String)
> :t f'
   f' :: Float -> (Float, String)
> :t g
   g' :: Float -> (Float, String)
> f' 1 2
   (3.0,"Added 1.0 and 2.0")
```

The article calls f' and g' "debuggable functions".

This is great, but now composing `f'` and `g'` is significantly more challenging. Where we could simply do `f.g` before,
`f'.g'` is not possible without some extra work.

```
> :t f.g
   f.g :: Float -> Float
> :t f'.g'
   <interactive>:1:4:
      Couldn't match type ‘(Float, String)’ with ‘Float’
      Expected type: Float -> Float
      Actual type: Float -> (Float, String)
      In the second argument of ‘(.)’, namely ‘g'’
      In the expression: f' . g'
```

Additionally, it would be really great if, when we composed these two functions,
debug information is preserved for all functions that are called.

We can construct our h' manually:

```
> let h' = \x -> (let (y,s) = g' x in let (z,t) = f' y in (z,s ++ ", " ++ t))
> :t h'
  h' :: Float -> (Float, [Char])
```

That is, h' takes an `x`, collects the resulting value and debug string `(y,s)` from `g'`, passes `y` to `f'` and then returns the result of `f'` and composes the debug strings.

The next exercise is to write a `bind` function that will compose these two functions for us. Basically a generic form of `h'`:

```
> let bind' = \f g x -> (let (y,s) = g x in let (z,t) = f y in (z,s ++ ", " ++ t)) :: (Float, String)
> :t bind'
bind'
  :: (t -> (t2, [Char])) -> (t1 -> (t, [Char])) -> t1 -> (t2, [Char])
```

This is moderately unpleasant to look at since we're not comfortable to slinging type variables around so freely, but this makes sense: this `bind'` function doesn't
care what types we're operating on, only that our `g` takes a `t` and returns a `t1` (bound to the variable `x`), that `f` takes a `t` and returns a `t2`. In the
case of our functions `f'` and `g'`, we see that all of the `tX` variables collapse to `Float`:

```
> :t bind' f' g'
bind' f' g' :: Float -> (Float, [Char])
```

The article implements `bind` slightly differently and so we will use their definition as well:

```
> let bind f' (gx,gs) = let (fx,fs) = f' gx in (fx,gs++fs)
> :t bind
bind :: (t -> (t1, [a])) -> (t, [a]) -> (t1, [a])
```

Their version of bind allows for any type of "debugging" output (where my `bind'` only allows `[Char]`, `bind` allows `[a]`), which is handy.
Thus I will redefine my debug output for `f'` and `g'` so they're cleaner when composed:

```
> let f' = \x -> (x+5, "Added " ++ (show x) ++ " and " ++ (show 5) ++ "\n") :: (Float,String)
> let g' = \x -> (x/2, "Divided " ++ (show x) ++ " by " ++ (show 2) ++ "\n") :: (Float,String)
```

And, in case we want to keep experimenting with `bind'`, we make that more general:

```
> let bind' = \f g x -> (let (y,s) = g x in let (z,t) = f y in (z,s++t))
> :t bind'
bind' :: (t -> (t2, [a])) -> (t1 -> (t, [a])) -> t1 -> (t2, [a])
```

Now, `bind` is different from my `bind'` in another way. My `bind'` was not really written in a functional style. Although you can partially apply
`f'` and `g'` to `bind'`, `bind` is more general and designed to only take one argument at a time. I am not yet a Haskell wizard so I will trust
that this is the superior way of doing things (indeed, it seems like a more general solution and therefore superior to only take one argument at a time if possible).

Now, the following two are equivalent:

```
> bind f' $ g' 4
(7.0,"Divided 4.0 by 2\nAdded 2.0 and 5\n")
> bind' f' g' 4
(7.0,"Divided 4.0 by 2\nAdded 2.0 and 5\n")
```

In fact, it's easy now to see that `bind` is the more general form, since `bind'` can be defined in terms of `bind`:

```
> let bind' f g x = bind f (g x)
> :t bind'
bind' :: (t -> (t1, [a])) -> (t2 -> (t, [a])) -> t2 -> (t1, [a])
> bind' f' g' 4
(7.0,"Divided 4.0 by 2\nAdded 2.0 and 5\n")
```

Now if I want to be lazy and omit `$`, I can.

More importantly, the article introduces us to the `*` symbol. Where we could compose `f` and `g` as `f.g`, we say that `f'` and `g'` are composed by a symbol `*`,
implemented here by `bind'` (but interestingly, not exactly by the more generic `bind`):

```
> :t f . g
f . g :: Float -> Float
> :t bind' f' g'
bind' f' g' :: Float -> (Float, [Char])
```

Next the article asks us to implement an identify function, a unit. Under normal composition, `f.id=f` and `id.f=f`, so we expect that this `unit=id'` has
the property `unit*f=f*unit=f*unit=f`.

This is simple to implement and verify (note that our original function `bind'`, which added commas between debug strings, made this identity impossible).

```
> let unit x = (x,"")
> :t bind unit
bind unit :: (t1, [Char]) -> (t1, [Char])
```

This actually isn't optimal since our `bind` and `bind'` expects `[a]` as debugging output, and our `unit` constrains us to only using `String`. Alas, we
will live with this for now. Let's verify that it has the expected behaviour:

```
> :t bind' unit f'
bind' unit f' :: Float -> (Float, [Char])
> bind' f' unit 4
(9.0,"Added 4.0 and 5\n")
> bind' unit f' 4
(9.0,"Added 4.0 and 5\n")
```

Not a rigorous proof, but it is what we'd expect.

Next we're introduced to the idea of `lift`: that is, turning _any_ function into a "debuggable" function.

```
> let lift f = unit . f
> :t lift
lift :: (a -> b) -> a -> (b, [Char])
> let lift = unit
> :t lift
lift :: (a -> b) -> a -> (b, [Char])
```

Next, in exercise three, we show that `lift f * lift g = lift (f.g)` (this is probably a monad law). Without being too rigorous:

```
> bind' (lift f) (lift g) 4
(7.0,"")
> (lift f.g) 4
(7.0,"")
```

This makes intuitive sense as well.

## Part Two: dealing with multivalued functions

I got very distracted trying to figure out how to accurately compute complex roots and cube roots. First, those definitions:

(This was fairly difficult to construct, as Haskell kept trying to construct infinite `Complex` types or `Complex (Complex a)`. Anyway, just
note that `Complex` is fairly tricky, at least for Haskell noobs like me. The core insight was, instead of multiplying `Float * Complex Float`, you need to multiply `Complex Float * Complex Float`;
and you can't get a `Complex Float` by multiplying `r * (1 :+ 0)` because then it expects `r` to be a `Complex Float` already. I realized that `(r :+ 0)` is equivalent, so that's what that means.)


```
> import Data.Complex
> let radius (a :+ b) = ((a ** 2) + (b ** 2)) ** (1/2)
> let theta (a :+ b) = (atan (b / a))
> polar (3 :+ 4) == (radius (3 :+ 4), theta (3 :+ 4))
True
> let i = (0 :+ 1)
> let extractRT' x = (radius x, theta x) :: (Float, Float)
> let extractRT x = let (r,t) = (extractRT' x) in ((r :+ 0), (t :+ 0)) :: (Complex Float, Complex Float)
> let cbrt x = let (r,t) = (extractRT x) in let r' = (r ** (1/3)) in let f x = (r' * exp(i * ((1/3)*t + x*pi))) in [f 0, f (2/3), f (-2/3)]
> cbrt (3 :+ 4)
[1.6289371459221758 :+ 0.5201745023045458,(-1.2649529063577512) :+ 1.1506136983844508,(-0.36398423956442405) :+ (-1.6707882006889963)]
> map (**3) $ cbrt (3 :+ 4)
[2.9999999999999996 :+ 3.9999999999999982,3.000000000000002 :+ 3.9999999999999942,2.999999999999998 :+ 4.0]
```

Then, the square root of a complex number
```
> let sqrt x = let e = exp 1 in let (r,t) = (extractRT x) in let z = ((r ** (1/2)) * (e ** (i * (t/2)))) in [z, -z]
> map (**2) $ sqrt (3 :+ 4)
[3.0 :+ 4.0,3.0000007 :+ 3.9999995]
```

Finally! (This part took several hours to write) So now we have our "multivalued functions". The next exercise is to write a `bind` that will allow us to compose these two functions.
What we want is:

```
> let x = (3 :+ 4)
> concat $ map cbrt $ sqrt x
[1.2920746 :+ 0.20129432,(-0.82036316) :+ 1.0183222,(-0.4717113) :+ (-1.2196164),1.2920746 :+ 0.20129432,(-0.82036316) :+ 1.0183222,(-0.4717113) :+ (-1.2196164)]
> let bind f x = concat $ map f $ x
> :t bind
bind :: (a1 -> [a]) -> [a1] -> [a]
> bind cbrt (sqrt x)
[1.2920746 :+ 0.20129432,(-0.82036316) :+ 1.0183222,(-0.4717113) :+ (-1.2196164),1.2920746 :+ 0.20129432,(-0.82036316) :+ 1.0183222,(-0.4717113) :+ (-1.2196164)]
```

Nice! The article implements bind exactly the same way, but slightly more concisely:

```
> let bind f x = concat (map f x)
```

Next, we need to define a unit, such that `bind unit (f x) = bind f (unit x) = f x`.

```
> let unit x = [x]
> (bind unit (sqrt x)) == bind sqrt (unit x)
True
> (bind sqrt (unit x)) == sqrt x
True
```

Next, we define `lift`, so that `f * g == (bind f) . g` and `lift f == lift (unit . f)`, and demonstrate that `lift` does what we expect.

Note, importantly, that when the article says `bind f . g`, this is equivalent to `(bind f) . g`, NOT `bind (f . g)`. This is easy to see, and the latter won't type-check anyway.

```
> let lift = unit
> :t (bind sqrt) . cbrt
(bind sqrt) . cbrt :: Complex Float -> [Complex Float]
```

## Haskell monad syntactic sugar

So at this point we've seen the same pattern twice: some data type and functions for which we define `bind` and `unit` - in the Haskell world, `unit` is known as `return`. As we've seen in
both examples, `lift` and `unit` are equivalent so we can think of `return` as being `unit` or `lift`, whichever makes more sense. Together, these should obey the Monad Laws (look on Wikipedia for these).
It is expected but not verified by GHCI.

It is then revealed that we have implemented all the components parts of a monad, twice. A monad is the 3-tuple `(m,unit,bind)` where `m` is the type-class `Debuggable`, `Multivalued`,
`Randomised`, etc. 

Back in the world of Haskell, we need to remember a few handy shorthands. `bind f x` is written `x >>= f`. And in order to override `return` and `>>=`, we need to use type classes.

## Rewriting our example in standard Haskell

In the standard Haskell world, our `Debuggable` class is really `Control.Monad.Writer`; `Multivalued` is `Control.Monad.List`; `Randomised` (which we did not implement above) is `Control.Monad.State`.

Let's experiment with using some of the built-in monad equivalents:

```
> :t \x -> writer (x+1, "inc.")
\x -> writer (x+1, "inc.")
  :: (MonadWriter [Char] m, Num a) => a -> m a
> :t return 7 >>= \x -> writer (x+1, "inc.")
 return 7 >>= \x -> writer (x+1, "inc.")
   :: (MonadWriter [Char] m, Num b) => m b
> :t runWriter
runWriter :: Writer w a -> (a, w)
> runWriter $ return 7 >>= \x -> writer (x+1, "inc.")
(8,"inc.")
```

Great! So let's dig into how `Writer` is implemented and see if we can make an equivalent `Debuggable` monad. This StackOverflow question has a great explanation of how
`Writer` works, and how to play with it in `ghci`: http://stackoverflow.com/questions/11684321/how-to-play-with-control-monad-writer-in-haskell

And, in my attempt to understand the "backwards arrow" `<-` and `do` syntax: https://wiki.haskell.org/Do_notation_considered_harmful https://wiki.haskell.org/Things_to_avoid#do_notation

The following is equivalent to our above example:

```
> :{
| runWriter $ do let x = 7
|                y <- writer (x+1, "inc.")
|                return y
| :}
(8,"inc.")
```

And the following two are equivalent:

```
> runWriter $ return 7
(7,())
> runWriter $ do return 7
(7,())
```

Now let's turn `Debuggable` into a monad:

```
> :{
newtype Debuggable a = Debuggable { runDebuggable :: (a,String) }
instance Monad Debuggable where
  return x = Debuggable(x,"")
  Debuggable (gx,gs) >>= f = let Debuggable(fx,fs) = f gx in Debuggable(fx,gs++fs)

:}
> runDebuggable $ return 7
(7,"")
> runDebuggable $ (return 5) >>= (\x -> Debuggable(x+5, "add"))
(10,"add")
```

Hooray!

The `newtype` syntax befuddled me for a while. The above `newtype` line is equivalent to:

```
data Debuggable a = Debuggable a String
runDebuggable :: Debuggable a -> (a,String)
runDebuggable (Debuggable a b) = (a,b)
```

Now for `Multivalued`:

```
> :{
newtype Multivalued a = Multivalued { runMultivalued :: [a] }
instance Monad Multivalued where
  return x = Multivalued x
  Multivalued (x:xs) >>= f = let y = map f (x:xs) in (Multivalued $ concat $ map (\(Multivalued (z:zs)) -> (z:zs)) y)

:}
> import Data.Complex
> runMultivalued $ return (3 :+ 4)
[3 :+ 4]
> let cbrt x = let (r,t) = (extractRT x) in let r' = (r ** (1/3)) in let f x = (r' * exp(i * ((1/3)*t + x*pi))) in Multivalued [f 0, f (2/3), f (-2/3)]
> let sqrt x = let e = exp 1 in let (r,t) = (extractRT x) in let z = ((r ** (1/2)) * (e ** (i * (t/2)))) in Multivalued [z, -z]
> let multiPow y x = Multivalued [x ** y]
> runMultivalued $ return (3 :+ 4) >>= sqrt >>= cbrt >>= multiPow 6
[3.0000012 :+ 4.000002,3.0000005 :+ 3.9999995,3.0000014 :+ 3.9999948,3.0000012 :+ 4.000002,3.0000005 :+ 3.9999995,3.0000014 :+ 3.9999948]
```

Huzzah! Amazing!

To finish these, we will need to `cabal install random` (in particular to `import System.Random` below).
