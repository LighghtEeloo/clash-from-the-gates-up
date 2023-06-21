# Clash From the Gates Up

This is a series of [Clash](https://clash-lang.org/) tutorial sessions for a hardware engineer. Or rather, an average bit-gate master who happens to be a Haskell infant. Therefore, a fairly gentle set of prerequisites is needed:

1. No Haskell knowledge is needed. It might be better to forget some of your system verilog experience or, even better, your C++ development skills. No imperative knowledge will do any good here - monads take time to grow into non-evil.
2. Some logic design background is needed. It would be pretty helpful if you could picture how all the circuit structures can be built into a larger device. It's recommended to recap the vending machine example (seriously?) for a self-check on state-machine background knowledge.
3. A brave heart.

See? We're friendly. Very friendly.

## Roles

In the following sessions, we'll play three roles: an engineer, a theorist, and an architect.

1. Engineer. I want to do this. Here's my plan, safe and sound. Ok, let's go.
2. Theorist. Why can I do this? How can I do that again generically, systematically, and ergonomically?
3. Architect. Here's how we can utilize these components in our superscale OoO processor project.

The engineer is frank, upright, and practical, always straight to the destination. Usable and practical examples are likely the engineer's work. The theorist wants more than a specific solution; a family of solutions is always welcome, and for such purpose, it's important to see at least a part of the mechanisms behind the scene. The architect governs the project, staring at the blueprints and proposing high-level tasks to bring them into reality. She also has a heart for organizing and cleaning up the codebase.

The paragraphs will be short and concise; within each, there'll be a consistent tone from one of us. Since time is running short for our four-week project, there won't be too much, and everything will be focused on quickly writing usable components.

I sincerely hope that you can enjoy this trip with the accompany of the three of us : )

## Table of contents

0. [Prelude: Waking Up, Loaded](0-prelude.md)
1. [Gateway Drug #1: Coding by Rewriting](1-drug.md)
2. [Circuit 101: Signal, Clock, Register, and Mux](2-wire.md)
3. [Have a Bit: Signed, Unsigned, Index, Bit, BitVector and BitPattern](3-bit.md)
4. [Algebraic Data Type: Type Constructors, Data Constructors and Patterns](4-data.md)

```
- how to compress valid bits into a better form

Gateway Drug #2: Moore and Mealy, in a few lines (Composability)

The Constitution Law for The Haskell Realm: Type Classes and Constraints

Inside-Out: Functor, Applicative, Bundle, Unbundle, and Alternative

Fashionable and Unreadable: Overdosing Operators

Vector: foldl foldr fold map and indexed version with indices and slice and split and shift

Compile Time Constants: KnownNat and SNat
- Index

Imperative Revisited: Monads and Monad Transformers

Unforgivably Convenient: Records and Baby Lens
```
