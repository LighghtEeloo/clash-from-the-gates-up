# Inside-Out: Functor, Applicative, Bundle, Unbundle, and Alternative

I was so close to changing the title of this session to `<$>`, `<*>`, and `<|>`. I'm kind of glad that I successfully resisted the strong but terrible temptation.

## Architect. How to translate logic into wires.

Three of the most important tasks of functional programming are to

1. free the programmers from trivial work
2. reuse code and logic (or in fact, just functions)
3. build abstractions to serve as better documentation

And Haskell is a top functional programming language (yeah I hear someone complaining on the other side of the planet; maybe that's from the inner me), so it should be really good like .. at these tasks.

Indeed. Let's talk about the nuclear weapon: type classes.

## Theorist. The Constitution Law for The Haskell Realm.

Rarely have I used metaphors in the titles. The *real* title should be "Type Classes and Constraints", but I simply can't resist the temptation again.

Type classes implement ad-hoc polymorphism, also called function overloading, where the overloaded behaviors are different for each type.

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















---

Feel free to advance to the [next session](6-operator.md).
