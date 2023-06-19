# Clash From the Gates Up

This is a series of [clash](https://clash-lang.org/) tutorial sessions for a hardware engineer. Or rather, an average bit-gate master who happens to be a Haskell infant. Therefore, a fairly gentle set of prerequisites is needed:

1. No Haskell knowledge is needed. It might be better to forget some of your system verilog experience or, even better, your C++ development skills. No imperative knowledge will do any good here - monads take time to grow into non-evil.
2. Some logic design background is needed. It would be pretty helpful if you could picture how all the circuit structures can be built into a larger device. It's recommended to recap the vending machine example (seriously?) for a self-check on state-machine background knowledge.
3. A brave heart.

In the following sessions, we'll play three roles: an engineer, a theorist, and an architect.

1. Engineer. I want to do this. Here's my plan, safe and sound. Ok, let's go.
2. Theorist. Why can I do this? How can I do that again generically, systematically, and ergonomically?
3. Architect. Here's how we can utilize these components in our superscale OoO processor project.

The paragraphs will be short and concise, within each there'll be a consistent tone from one of the three of us. Since time is running short for our four week project, there won't be too much and everything will be focused on quickly writing usable components.



## Table of contents

0. [Prelude: Waking Up, Loaded](./0-prelude.md)
1. [Gateway Drug #1: Coding by Rewriting](1-drug.md)
2. [Machine 101: Signal, Register and Clock](2-wire.md)

```
Inside-Out: Functor, Applicative, Bundle, Unbundle, and Alternative

Gateway Drug #2: Moore and Mealy, in a few lines (Composability)

Have a Bit: Signed, Unsigned, Index, Bit and BitVector

Algebraic Data Type: Types and Data Constructors and Patterns
	- how to compress valid bits into a better form

Forking Paths: Branching, Pattern Matching and Guards, a.k.a. Control Flow
	- if then else case and the freedom of declarative programming

Fashionable and Unreadable: Overdosing Operators

Vector: foldl foldr fold map and indexed version with indices and slice and split and shift

Compile Time Constants: KnownNat and SNat
	- Index

The Constitution Law for The Haskell Realm: Constraints

Imperative Revisited: Monads and Monad Transformers

Unforgivably Convenient: Records and Baby Lens
```
