# Fashionable & Unreadable: Overdosing Operators

Operators are **fun**. Operators are hard to read - unless we understand the meaning behind the symbol.

## Theorist. Operators are functions.

Let's focus on binary operators first. For example, the `+` operator.

To use it, just place it between the operands.

```haskell
1 + 1
```

But actually, it's a function that takes two inputs and returns an output. Place parentheses around the operator to use it as a function name.

```haskell
(+) 1 1
```

And it's indeed a function name. So functional that you can define it.

```haskell
(+.) :: Float -> Float -> Float
(+.) x y = x + y
```

## Theorist. Function Composition.

