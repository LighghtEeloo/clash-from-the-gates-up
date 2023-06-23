# Inside-Out: Functor, Applicative, Alternative, Bundle, and Unbundle

I was so close to changing the title of this session to `<$>`, `<*>`, and `<|>`. I'm kind of glad that I successfully resisted the strong but terrible temptation.

## Architect. How to translate logic into wires.

Three of the most important tasks of functional programming are to

1. free the programmers from trivial work
2. reuse code and logic (or, in fact, just functions)
3. build abstractions to serve as better documentation

And Haskell is a top functional programming language (yeah, I hear someone complaining on the other side of the planet; maybe that's from the inner me), so it should be really good like .. at these tasks.

Indeed. Let's talk about the nuclear weapon: type classes.

## Theorist. The Constitution Law for The Haskell Realm.

Rarely have I used metaphors in the titles. The *real* title should be "Type Classes and Constraints", but I simply can't resist the temptation again.

Type classes implement ad-hoc polymorphism, also called function overloading, where the overloaded behaviors differ for each type.

Type classes are **not** types; type classes are *constraints* on *types*. Beginners can easily make the mistake of placing constraints in the position of types. It's just wrong. `System` is a type; `KnownDomain dom` is a constraint; therefore, `KnownDomain System` holds. There are no means to replace `System` by `KnownDomain`. We can use `:info` to see what a name is; if it's `data` or `type`, then it's a type; if it's `class`, then it's a type class, or you can also call it a constraint.

Again, we can start with small examples.

## Engineer. Can I describe things that can turn into `Bool`?

I'm sure you've written `while (1)`, but why can C understand that `1` can be a boolean? Well, why not?

```haskell
class Boolable a where
  toBool :: a -> Bool
```

And it's easy to implement different instances for `Boolable`:

```haskell
instance Boolable Bool where
  toBool = id
instance Boolable (Maybe a) where
  toBool (Just _) = True
  toBool Nothing = False
instance Boolable (Either a b) where
  toBool (Left _) = False
  toBool (Right _) = True
```

.. to which we can apply `if`:

```haskell
if toBool True then .. else ..
if toBool Nothing then .. else ..
if toBool (Right 1) then .. else ..
```

We can notice that `toBool` can take in different types of inputs, but it can only choose from all instances that have implemented `Boolable`.

## Engineer. I can reuse logic on wires!

Suppose I have two wires `Signal dom (Unsigned 64)`, and I have a function

```haskell
double :: Unsigned 64 -> Unsigned 64
double a = 2 * a
-- double = (*) 2
```

And I can reuse the pure function on wires!

```haskell
doubleWires :: (KnownDomain dom) =>
  Signal dom (Unsigned 64) ->
  Signal dom (Unsigned 64)
doubleWires a = double <$> a
-- doubleWires a = fmap double a
```

---

Suppose I have two wires `Signal dom (Unsigned 64)`, and I have a function

```haskell
doubleAdd :: Unsigned 64 -> Unsigned 64 -> Unsigned 64
doubleAdd a b = 2 * a + b
```

And I can reuse the pure function on wires!

```haskell
doubleAddWires :: (KnownDomain dom) =>
  Signal dom (Unsigned 64) ->
  Signal dom (Unsigned 64) ->
  Signal dom (Unsigned 64)
doubleAddWires a b = doubleAdd <$> a <*> b
-- doubleAddWires a b = (doubleAdd <$> a) <*> b
-- doubleAddWires a b = liftA2 doubleAdd a b
```

Ok, this is perhaps confusing. Let's invite the theorist.

## Theorist. Functor.

The definition of `class` `Functor` is

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
```

and `<$>` is, in fact, just `fmap`:

```console
clashi> :i (<$>)
(<$>) :: Functor f => (a -> b) -> f a -> f b
        -- Defined in `Data.Functor'
infixl 4 <$>
```

and here's an instance of `Functor (Maybe a)`:

```haskell
instance Functor (Maybe a) where
  fmap g Nothing = Nothing
  fmap g (Just x) = Just (g x)
```

When we are confused, it doesn't mean we don't remember what we've seen; we're just unable to interpret what we've seen. So here's a perspective to look at it.

Things that are functors are containers that we can operate on. 

For example, `Maybe`. Try in `clashi`:

```console
clashi> double x = 2 * x
clashi> :type fmap double
clashi> fmap double (Just 1)
clashi> fmap double Nothing
clashi> double <$> (Just 1)
```

`Maybe` is a container in the sense that it could contain something (or not). If you have a function that operates on the thing `a` that lives inside it, you can operate on `Maybe a` by `fmap`ping that function.

Check out the info of `Functor`; use Google when necessary.

```console
clashi> :info Functor
```

You can see that many old friends of ours have implemented instances of `Functor`, which means you can use `<$>` on them.

Questions?

## Engineer. Apply `Functor` on `Signal`s.

Since `Signal` is, in some sense, a container, it's also a `Functor`.

Let's recall the examples above.

> Suppose I have two wires `Signal dom (Unsigned 64)`, and I have a function

```haskell
double :: Unsigned 64 -> Unsigned 64
double a = 2 * a
-- double = (*) 2
```

> And I can reuse the pure function on wires!

```haskell
doubleWires :: (KnownDomain dom) =>
  Signal dom (Unsigned 64) ->
  Signal dom (Unsigned 64)
doubleWires a = double <$> a
-- doubleWires a = fmap double a
```

Nothing too hard, right?

## Theorist. Applicative.

.. maybe just consult the engineer. We don't have much time today.

## Engineer. Apply `Applicative` on`Signal`s.

Let's recall the examples above.

.. I guess continuously quoting myself is not a good habit, is it?

Basically, if a function `g` takes multiple arguments and it's curried, you can `liftA` it to make it take `f a` and `f b` instead of `a` and `b` if `Applicative f`. `f` here is the container-ish thing. As for the operators, you can use

```haskell
doubleAdd <$> (Just 1) <*> (Just 2)
```

Try the following in `clashi`:

```console
clashi> doubleAdd a b = 2 * a + b
clashi> doubleAdd <$> (Just 1) <*> (Just 2)
clashi> doubleAdd <$> (Just 1) <*> Nothing
clashi> doubleAdd <$> Nothing <*> (Just 2)
clashi> doubleAdd <$> Nothing <*> Nothing
```

Hopefully, this makes sense to you.

## Theorist. Alternative.

Another handy tool for `Maybe`. It's just shortcircuit-or.

Observe:

```console
clashi> Just 1 <|> Just 2
clashi> Just 1 <|> Nothing
clashi> Nothing <|> Just 2
clashi> Nothing <|> Nothing
```

and check

```console
clashi> :info (<|>)
```

There you got it.

## Theorist. Bundle and Unbundle.



---

Feel free to advance to the [next session](7-operator.md).
