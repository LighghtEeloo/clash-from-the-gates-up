# Machine 101: Signal, Clock, Register, and Mux

## Engineer. I want a constant signal.

Easy.

```haskell
import Clash.Prelude

alwaysHigh :: Signal dom Bit
-- or better, `(KnownDomain dom) => Signal dom Bit`
alwaysHigh = pure 1
-- or `pure high` in clash style
```

The first thing you'd like to do in a clash project is to `import Clash.Prelude`. Then you can use all common clash symbols, like `KnownDomain`, `Signal`, `Bit`, and `high`.

Exercise: implement `alwaysLow`.

And it's even possible to output a metastable signal deliberately.

```haskell
metaStable :: Signal dom Bit
metaStable = undefined
```

Life is never easy. If you feel stuck, you can leave an `undefined` there and fill it in later.

## Theorist. Domain.

What is `dom` in the examples above?

First of all, staring at the syntax, we can categorize it as a type variable. Sure. But what does it stand for?

The type system can save us here. If we consider `(KnownDomain dom) => Signal dom Bit`, the better type ascription, we'll see that `dom` must satisfy a type constraint called `KnownDomain`.

## Theorist. Name Convention.

Haskell variables use `smallCamelCase`. Haskell types use `BigCamelCase`, and so do constraints. Haskell names can include `'` as long as it's not the first character. Haskell operators can be function names by adding parentheses around them, like `(+) 1 1`.

## Engineer. I want to wire things up.

Wires are functions on `Signal`s.

```haskell
wire :: (KnownDomain dom, NFDataX a) => Signal dom a -> Signal dom a
wire x = x
```

A wire has an input and an output. `x` on the LHS is the input, `x` on the RHS is the output. Ignore the type declaration for now. See the chapter on Functor.

Now that I have two wires. I can wire them up and make them one wire again.

```haskell
longerWire :: (KnownDomain dom, NFDataX a) => Signal dom a -> Signal dom a
longerWire x = wire (wire x)
longerThanLongerWire :: (KnownDomain dom, NFDataX a) => Signal dom a -> Signal dom a
longerThanLongerWire x = wire (wire (wire x))
```

See the chapter on operators to check out and function composition.
