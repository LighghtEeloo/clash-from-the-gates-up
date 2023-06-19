# Clash From the Gates Up

This is a series of clash tutorial sessions for a hardware engineer. Or rather, an average bit-gate master who happens to be a Haskell infant. Therefore, a fairly gentle set of prerequisites is needed:

1. No Haskell knowledge is needed. It might be better to forget some of your system verilog experience or, even better, your C++ development skills. No imperative knowledge will do any good here - monads take time to grow into non-evil.
2. Some logic design background is needed. It would be pretty helpful if you could picture how all the circuit structures can be built into a larger device. It's recommended to recap the vending machine example (seriously?) for a self-check on state-machine background knowledge.
3. A brave heart.

In the following sessions, we'll play three roles: an engineer, a theorist, and an architect.

1. Engineer. I want to do this. Here's my plan, safe and sound. Ok, let's go.
2. Theorist. Why can I do this? How can I do that again generically, systematically, and ergonomically?
3. Architect. Here's how we can utilize these components in our superscale OoO processor project.

The paragraphs will be short and concise, within each there'll be a consistent tone from one of the three of us.



```
Wire, Register and Clock

Signal: Functor, Applicative and Alternative

Have a Bit: Signed Unsigned Index Bit BitVector

Algebraic Data Type: Type and Data Constructors and Pattern
	- how to compress valid bits into a better form

Control Flow (Forking Paths): Branching, Pattern Matching and Guards
	- if then else case and the freedom of declarative programming

Vector: foldl foldr fold map and indexed version with indices and slice and split and shift

Compile Time Constants: KnownNat and SNat
	- Index

Constraint: The Constitution Law for The Haskell Realm

Monads: Imperative Revisited

Operators and Baby Lens
```
