# Fashionable & Unreadable: Overdosing Operators

> Operators are **fun**. Operators are hard to read - unless we understand the meaning behind the symbol.
>
> \-\- Theorist. Operators. *Gateway Drug #1: Coding by Rewriting*

Let's still focus on binary operators because they are really fun.

## Theorist. `$`.

Recall the trivial `longerThanLongerWire` you wrote a long time ago.

```haskell
longerThanLongerWire :: (KnownDomain dom, NFDataX a) => Signal dom a -> Signal dom a
longerThanLongerWire x = wire (wire (wire x))
```

The following are all valid.

```haskell
longerThanLongerWire x = wire $ wire (wire x)
longerThanLongerWire x = wire (wire $ wire x)
longerThanLongerWire x = wire $ wire $ wire x
```

The `$` operator saves you from the hell of parentheses by changing the precedence. Observe how it cuts off the need for adding bothering parentheses.

## Theorist. `.`.

`.` stands for function composition. 

```haskell
longerThanLongerWire x = wire . wire . wire x
```

