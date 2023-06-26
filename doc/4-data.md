# Algebraic Data Type: Type Constructors, Data Constructors, and Patterns

## Architect. Algebraic Data Type.

The ever-existing headache for all programmers is the problem of encoding. When "coding," we encode information from one form into another. Programming languages have equipped us with *data types* to describe different representations of the encodings.

Three sorts of representations stand out as fundamental:

1. Collection, or Cartesian Product, which we call `struct` or `std::tuple` in C++
2. Dataflow, or functions, which we call `std::function` in C++
3. Possibilities, or ..?, which we call `union` in C++

The need to represent possibilities always exists when designing hardware. For example, `instruction`s could be easily sorted into different types, where each has different parameters; however, they need to fit into the same length of bits. This is a hardware version of `union` - or tagged union, where the tag is the bits representing the type of instruction.

We've all seen how decoders work and how long it takes. What if we can get rid of decoders while preserving the ability to tell different types of instructions apart? Well, the algebraic data type (ADT) is the answer from PL people.

````haskell
data Instruction
  = RInst RInstruction
  | SInst SInstruction
  | IInst IInstruction
  | UInst UInstruction

-- And define these Instructions elsewhere
data RInstruction = -- ..
data SInstruction = -- ..
data IInstruction = -- ..
data UInstruction = -- ..
````

We'll invite the Engineer to get us into the details.

## Engineer. I want a null pointer.

The type is called `Maybe` in Haskell. It's something you can define, but Haskell has prepared it for you:

```haskell
data Maybe a
  = Just a
  | Nothing
```

And a null pointer of type `Maybe Int` is `Nothing`; if it points to the value `1`, it's `Just 1`.

`Maybe` is a type constructor. `a` is a type variable. `Maybe Int` is a type, and so is `Maybe Bool`.

`Just` and `Nothing` are data constructors.

What's that?

## Theorist. Data Constructor.

Data constructors can take arbitrarily many types, for example

```haskell
data RInstruction = RInstrunction ROp Rd RS1 RS2
data ROp = SLL | SRL | SRC -- ..
type Rd = Reg
type RS1 = Reg
type RS2 = Reg
type Reg = Unsigned 5
```

Or, instead of defining a new `RInstruction`, we can write

```haskell
data Instruction
  = RInstruction ROp Rd RS1 RS2
  -- ..
```

since data constructors are convenient.

Also, notice that data constructors can have the same name as the type constructors, like `RInstruction`.

And what is `type`?

## Theorist. Type Alias.

`type` indicates type alias, like `using` or `typedef` in C++. It's not defining a new type but reusing existing ones. Therefore, there're no new data constructors defined. All `Rd`, `RS1`, `RS2`, and `Reg` can be replaced with `Unsigned 5`.

There's also `newtype`, which is **not** type alias. You don't need to write it yourself; HLS will tell you when you can do it.

## Engineer. How to create and consume ADTs?

It's more intuitive and concise than what you may have seen in other languages than in Haskell.

You've seen how to create, or construct, ADT:

```haskell
Just 1
Nothing
```

Basically, you can place data constructors upfront, and place all arguments by order.

To use it, or destruct it, is also easy:

```haskell
isOne :: Maybe Int -> Bool
isOne (Just 1) = True
isOne (Just _) = False
isOne Nothing = False
```

or,

```haskell
isOne :: Maybe Int -> Bool
isOne (Just 1) = True
isOne _ = False
```

or, remember the powerful pattern matching,

```haskell
isOne :: Maybe Int -> Bool
isOne one =
  case one of
    (Just 1) -> True
    _ -> False
```

or,

```haskell
isOne :: Maybe Int -> Bool
isOne =
  \case
    (Just 1) -> True
    _ -> False
```

These data constructors are used as *pattern*s.

---

Tuple is also ADT, with special syntax support. Try these in `clashi`:

```console
clashi> :type (1, True)
clashi> (,) 1 True
clashi> :type (,)
clashi> :type (,,)
clashi> :type (,,,)
```

`(,)` is a data constructor. You know how to destruct it already. There're also `fst` and `snd` for convenience, but you don't need to use them.

 ## Architect. Frequently used data types.

We've seen `Maybe` for the null pointer, or it can be seen as something that may or may not exist.

There's also `Either`, which means it could be something or another.

```haskell
data Either a b = Left a | Right b
```

Observe how data types can be defined in one line. It's really just a matter of style.

But you won't know how most Haskell Programmers commonly use `Either`. For them, `Either` stands for the result of an operation; it could *either* be a failure (`Left`) or success (`Right`) since something is *either* done *right* or not. We'll use it, so be prepared.

---

There's also `[]`, or `List`, or linked-list, but we won't use it frequently in our hardware project.

```haskell
data [] a = [] | a : [a]
```

A similar definition would be

```haskell
data List a = Nil | Cons a (List a)
```

but that's not so cool if Haskellers explain it too clearly.

We're mentioning it because this is why type annotation uses `::` instead of `:`. These language designers thought link-list cons would be used more often than type annotation! And now we're suggested to annotate every single one of the top-level definitions.

History is just hard to believe, isn't it?

---

Feel free to advance to the [next session](5-class.md).
