# Circuit 101: Signal, Clock, Register, and Mux

Now that we've seen how Haskell works in general (except the type system part), we can see how we can apply the knowledge and build up our circuit. But first, we need to learn some basic components.

## Engineer. I want a constant signal.

Easy.

```haskell
import Clash.Prelude

alwaysHigh :: Signal dom Bit
-- or better, `(KnownDomain dom) => Signal dom Bit`
alwaysHigh = pure 1
-- or `pure high` in clash style
```

The first thing you'd like to do in a Clash project is to `import Clash.Prelude`. Then you can use all common Clash symbols, like `KnownDomain`, `Signal`, `Bit`, and `high`.

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

The type system can save us here. If we consider `(KnownDomain dom) => Signal dom Bit`, the better type annotation, we'll see that `dom` must satisfy a type constraint called `KnownDomain`.

---

A `Domain` is an area of components that share the same clock, reset and enable signals. We only need to deal with one domain during this project, but imagine speeding up memory by setting a faster clock frequency, which will create a different domain from the processor.

`System` is a Haskell type and is the only domain we'll use. It's the "main" domain. Since `dom` here is a type parameter, we can place any domain in, for example, `System`.

We'll talk more about type constraints later; for now, you only need to know conceptually what domain is.

---

Why would domains be useful? Good question. Domains can hide unimportant details. Remember that you have to pass in the clock, reset, and enable signals everywhere in the System Verilog? You can say that a `dom` satisfies `HiddenClockResetEnable dom` and no longer prepend `Clock dom -> Reset dom -> Enable dom -> ...` before you do anything!

```console
:info HiddenClockResetEnable
```

See more by yourself.

## Architect. Naming Convention.

Haskell variables use `smallCamelCase`. Haskell types use `BigCamelCase`, and so do Haskell constraints. Haskell names can include `'` as long as it's not the first character. Haskell operators can be function names by adding parentheses around them, like `(+) 1 1`.

## Engineer. I want to wire things up.

Wires are functions on `Signal`s.

```haskell
wire :: (KnownDomain dom, NFDataX a) => Signal dom a -> Signal dom a
wire x = x
```

A wire has an input and an output. `x` on the LHS is the input, and `x` on the RHS is the output. Ignore the type declaration for now. See the chapter on Functor.

Now that I have two wires. I can wire them up and make them one wire again.

```haskell
longerWire :: (KnownDomain dom, NFDataX a) => Signal dom a -> Signal dom a
longerWire x = wire (wire x)
longerThanLongerWire :: (KnownDomain dom, NFDataX a) => Signal dom a -> Signal dom a
longerThanLongerWire x = wire (wire (wire x))
```

See the chapter on operators to check out `$` and function composition.

## Engineer. How to build a counter with `register`.

The secret sauce is to use a register. Let's find out what its interface looks like.

```console
ghci> :type register
ghci> :info register
```

Observe how the outputs differ for `:type` and `:info`.

A register takes an initial value and an input signal and produces an output signal.

```haskell
counter :: (HiddenClockResetEnable dom) => Signal dom (Unsigned 4) -- [0, 16) counter
counter = state
  where
    state = register 0 nextState
    nextState = state + 1
```

Or if you are a one-liner:

```haskell
counter = register 0 (counter + 1)
```

`Unsigned 4` is a type for a sized number. See the next chapter for details.

## Engineer. How to reset the counter with `mux`.

`mux` is like `if` on `Signal`s. It takes a `Signal dom Bool` and two `Signal dom a` for `True` and `False` branches.

```haskell
counter ::
  (HiddenClockResetEnable dom) =>
  Reset dom -> Signal dom (Unsigned 4) -> Signal dom (Unsigned 4) -- [0, 16) counter
counter rst d = state
  where
    state = register 0 nextState
    nextState = mux (unsafeToHighPolarity rst) d (state + 1)
    -- If reset is high, then output `d` (default), otherwise it's a counter
```

Check [`unsafeToHighPolarity`(link)](https://hackage.haskell.org/package/clash-prelude-1.6.4/docs/Clash-Signal.html#v:unsafeToHighPolarity) to see how to to work with reset.

## Engineer. Turn `fib` into a circuit.

Till now, all components are ready for you to build a circuit. So why not build one?

Write a function `fib` which is a circuit. It should have a `Signal dom (Unsigned 8)` as input and `Signal dom (Unsigned 64)` as output.

Recall the last version of `fibState`, a tail-recursive function, and then..

```haskell
fibState :: (Int, Int) -> Int -> Int
fibState (a, _) 0 = a
fibState (_, b) 1 = b
fibState (a, b) n = fib (n - 1) (b, a + b)
```

..rewrite it into a state transfer function.

```haskell
fibStateTrans ::
  (Int, Int) -> -- last (a, b)
  Int -> -- last n
  (Int, Int, Int) -- next (a, b, n)
fibStateTrans (a, b) 0 = (a, b, 0)
fibStateTrans (a, b) 1 = (a, b, 1)
fibStateTrans (a, b) n = (b, a + b, n - 1)
```

Then.. think a little about how to do it.

---

Feel free to advance to the [next session](3-bit.md).
