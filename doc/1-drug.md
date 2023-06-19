# Gateway Drug #1: Coding by Rewriting

"Haskell is an elegant language; therefore, it's a good choice for any coding." They say, from time to time.

## Theorist. Declarative Programming.

Observe: the infamous [Fibonacci sequence](https://en.wikipedia.org/wiki/Fibonacci_sequence), in Haskell.

```haskell
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
```

We're defining the `fib` function which takes an integer and returns an integer, or in Haskell, `Int -> Int`. Here every line is a piece of definition, and we need to take all lines into account for the function to be meaningful. Each line is responsible for a case of input ranging from `0`, `1` or `n`, from top to bottom. `n` is a variable that covers all other cases that is not `0` or `1`. It will be called if the input is neither `0` nor `1`. To use it, you can call the function like `fib 9`, and the result will be `34`. If you evaluate `fib 8` or `fib 10`, the result will be `21` or `55`, accordingly.

This is actually a slow version, but it's definitely trivial to write.

（你就说短不短吧）

## Theorist. Term Rewriting.

Now that you've understood how `fib` works, let's talk about term rewriting. Programming is just like continuously rewriting something from one form to another. In Haskell, we declare rules to describe the rewrite process. 

The `=` sign is divine: it divides the declaration into the left hand side and the right hand side, proclaiming a rule of rewrite. For example, if you meet `fib 0`, rewrite it to `1`; if `fib 1`, `1`; otherwise, suppose you meet `n` after `fib`, rewrite it to `fib (n - 1) + fib (n - 2) `, and then do it again for the new sub-`fib`s.

## Theorist. Type Declaration.

You can add type declarations for fun and safety. Mostly fun, of course.

```haskell
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
```

Just kidding. You should add it for every top-level term declaration, and the compiler will yell at you if you don't. But no, I wasn't kidding; I was just saying that most terms are __fun__ctions.

"`::` is hard to type!"

Yeah, the language designers think the same, but they designed `::` to be type declaration delibrately anyways. Because they thought a _real_ type system should be invisible to the users and understand every assumption the programmer wants to say with little or no help from the programmer. Look at how things are changing in decades.

## Theorist. Layout.

Fun fact: you can concat all lines into one with `;`

```haskell
fib 0 = 0; fib 1 = 1; fib n = fib (n - 1) + fib (n - 2)
```

Haskell has interesting decisions around layout (whitespaces, indentations, and line breaks).

## Theorist. `if` and `case`.

Observe: an equivalent version of `fib`, also in Haskell.

```haskell
fib n =
	if n == 0 then 0
	else if n == 1 then 1
	else fib (n - 1) + fib (n - 2)
```

And another.

```haskell
fib n =
	case n of
		0 -> 0
		1 -> 1
		n -> fib (n - 1) + fib (n - 2)
```

Never mess around with layout. If you see "parse error" that's likely something about layout.

## Conclusion

"Haskell is an elegant language; therefore, it's a delightful choice for any coding." They say, from time to time. Well, they're lying, and they must have been top liers. Because the first half is correct - well, until you're actually working on a project in Haskell.
