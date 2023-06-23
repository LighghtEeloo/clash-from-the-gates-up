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

Now let's check out the type of `$`.

```console
clashi> :type ($)
clashi> :info ($)
```

What do you mean .. it does nothing?

```console
($) :: (a -> b) -> a -> b       -- Defined in `GHC.Base'
infixr 0 $
```

Yes, it almost does nothing, except ..

## Theorist. ` `.

Yeah, there's a space in the title, and it's not a typo.

, or function application, is perhaps the most overlooked operator in functional programming languages. Unlike `(,)`, we can't check it in `clashi` this time because it's too deeply coupled into the Haskell syntax. But if we want to write out its type, it's exactly the same as `$`; the only difference is its precedence and associativity. It has the **highest** precedence, or we can say it **binds the tightest** among all operators.

Observe:

```haskell
id x + y
-- (id x) + y
x + id y
-- x + (id y)
id $ x + y
-- id (x + y)
```

That's it. That's how the most important operator in all FP languages works.

## Theorist. `|>`.

I made this up in the `Utils` module because I really like it.

```haskell
(|>) :: a -> (a -> b) -> b
(|>) a f = f a
```

We'll see how it works in the next chapter.

## Theorist. `.`.

`.` stands for function composition.

```haskell
longerThanLongerWire x = (wire . wire . wire) x
```

Or recall `$`:

```haskell
longerThanLongerWire x = wire . wire . wire $ x
```

And recall the eta law (or look at compiler's lints):

```haskell
longerThanLongerWire = wire . wire . wire
```

It's OK if you don't know what the eta law is. It's not important - the compiler will catch it for you.

## Theorist. Infix Operators.

Operators are **fun**. True. But do you know **fun**ctions can be operators?

Any function that takes two inputs can be directly used as an infix operator:

```haskell
add x y = x + y
1 `add` 2
-- 3
```

Kind of exciting, right? Can't wait to see how I can mess up the code base!

## Architect. No, you don't.

You should use operators with care and with caution. Check your thoughts on new operators with another teammate before using something too fancy. If your teammate can't follow your logic, try to avoid using it.

---

Feel free to advance to the [next session](8-vector.md).
