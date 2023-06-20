# Gateway Drug #1: Coding by Rewriting

"Haskell is an elegant language; therefore, it's a good choice for any coding." They say, from time to time.

## Theorist. Declarative Programming.

Observe: the infamous [Fibonacci sequence](https://en.wikipedia.org/wiki/Fibonacci_sequence) in Haskell.

```haskell
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
```

We're defining the `fib` function, which takes an integer and returns an integer, or in Haskell, `Int -> Int`. Here every line is a definition, and we need to consider all lines for the function to be meaningful. Each line is responsible for a case of input ranging from `0`, `1`, or `n`, from top to bottom. `n` is a variable that covers all other cases that is neither `0` nor `1`. It will be called if the input is again neither `0` nor `1`. To use it, you can call the function `fib 9`, and the result will be `34`. If you evaluate `fib 8` or `fib 10`, the result will be `21` or `55`.

This is actually a slow version, but it's definitely trivial to write.

（你就说短不短吧）

## Theorist. Term Rewriting.

Now that you've understood how `fib` works, let's discuss term rewriting. Programming is just like continuously rewriting something from one form to another. In Haskell, we declare rules to describe the rewrite process. 

The `=` sign is divine: it divides the declaration into the left-hand side (LHS) and the right-hand side (RHS), proclaiming a rule of rewrite. For example, if you meet `fib 0`, rewrite it to `1`; if `fib 1`, `1`; otherwise, suppose you meet `n` after `fib`, rewrite it to `fib (n - 1) + fib (n - 2) `, and then do it again for the new sub-`fib`s.



Note that Haskell functions are naturally recursive. It means that you can use `fib` when defining it.

Now, a small question for you. What if I write this:

```haskell
x = x
```

What is `x`? Recall the idea of term rewriting and say what will happen if we evaluate `x`.

## Theorist. Type Declaration.

You can add type declarations for fun and safety. Mostly fun, of course.

```haskell
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
```

Just kidding. You should add it for every top-level term declaration, and the compiler will yell at you if you don't. But no, I wasn't kidding; I was saying that most terms are **fun**ctions.



`::` is more widely used than top-level type declarations. In fact, you can use it for any type ascription.

```haskell
mysterious = 42 :: Float
```

Here `42 :: Float` is a number `42` explicitly annotated as a `Float`. And we define `mysterious` to be `42 :: Float`, which is a `Float`.



"`::` is hard to type!"

Yeah, the language designers think the same, but they designed `::` to mean type ascription deliberately anyways. Because they thought a *real* type system should be invisible to the users and understand every assumption the programmer wants to say with little or no help from the programmer.

Look at how things are changing in decades.



Here's another question for you.

```haskell
x = x
```

What's the type of `x`?

## Engineer. Figuring out things without using Google.

It's possible to let Haskell tell us the answers to the previous questions.

```shell
cabal run clashi
```

This line of command will summon a clash interactive shell. It's now possible to declare, evaluate, and inspect the type of any term.

```haskell
x = x
:type x
x
```

`:type` is an interactive command that won't work in the source code. You can use the short version `:t`. It shows a type of term.

The other useful command is `:info` or `:i` for short. It shows information about types, type classes, and operators.

If you are stuck, press `Ctrl+C`, which sometimes is written as `^C`, since `Ctrl` can be written as `^`.

If you want to exit, press `Ctrl+D` (`^D`).

## Theorist. Types and Type Variables.

In the last session, you should have seen the output

```console
ghci> :type x
x :: t
```

It indicates that `x` has type `t`. Wait, say that again?

To understand the result, we need to know the rules of the Haskell type system better.

1. All type literals begin with uppercase. `Int` and `Float` are good examples.
2. If you see a type that starts with lowercase, pay attention to where it's introduced. Because unlucky, it could either come from a binder before or get implicitly defined right on site! 
   - If bound earlier, it will be replaced with a type literal later.
   - If defined on-site, it means "forall type that is later plugged in here, the following holds". It's like the generic type in C++.

This is one of the most controversial designs in the Haskell world. If you're lost, let's see some examples.

```haskell
x :: t -- it means `t` is any type that is introduced right on-site

b :: Bool
b = x -- works out fine

i :: Int
i = x -- also works out fine
```

And the simplest terms you can find in Haskell:

```console
:type id
id 42
:type const
const 42 10000
:type undefined
undefined
const 42 undefined
```

Try these in the interactive shell and observe.

Note that using `a`, `b`, `c`, and so on is common for referring to type variables with little meaning. Like `template <typename T>` in C++.

## Theorist. Layout.

Fun fact: you can concat all lines into one with `;`

```haskell
fib 0 = 0; fib 1 = 1; fib n = fib (n - 1) + fib (n - 2)
```

And it's possible to add many line breaks and indentations.

```haskell
fib 0 =
	0

fib 1 =
	let
		x = 1
	in
		x

fib n = x + y
	where
		x = fib (n - 1)
		y = fib (n - 2)
```

Many line breaks are identical to one. But indentations are necessary.

Haskell has interesting decisions around layout (whitespaces, indentations, and line breaks). When refactoring the layout, do only what you know you're doing.

What if I don't care once it's working? Well, just run the formatter.

## Architect. Formatting.

In vscode, use the built-in `Format Document` utility.

Run `ormolu -i $(find . -name "*.hs")` to format all Haskell files in the repo.

## Engineer. `if`, `case`, and guards.

Is there a good old function declaration like that in C?

If you are shocked by the example above, here's an equivalent version of `fib`, also in Haskell.

```haskell
fib' :: Int -> Int
fib' n =
	if n == 0 then 0
	else if n == 1 then 1
	else fib' (n - 1) + fib' (n - 2)
```

Here `fib'` is just a normal variable name. You can change it to `goodFib` or `fib_` as you like. Or `f'i'b`. But not `'fib`. Also, you need to indent when writing a definition of a function.



And another. But quite different in fashion, closer to the original `fib`.

```haskell
fib'' :: Int -> Int
fib'' n =
	case n of
		0 -> 0
		1 -> 1
		n -> fib'' (n - 1) + fib'' (n - 2)
```

It's called pattern matching, similar to `switch` in C but way more powerful and expressive. But remember, `case` needs indentation. Never, ever mess around with the layout. If you see "parse error", that's likely something about layout.



And another. But cursed.

```haskell
fib''' :: Int -> Int
fib''' n | n == 0 = 0
fib''' n | n == 1 = 1
fib''' n = fib''' (n - 1) + fib''' (n - 2)
```

The `|` here is called the guard. It means this branch of declaration is matched only if the boolean after `|` is `True`. Remember, `=` is divine, but `==` is not. It's "cursed" because the use of guards is pretentious.

## Engineer. Lambda Functions.

What if I want a function in place without explicitly naming it?

Well, there is a way.

```haskell
id' = \x -> x
```

Here `\` is the symbol for lambda, which stands for functions that don't have a name. It's a more general way of describing functions.

We can have our alternative `fib` (since you've understood what names are allowed for variables, let's avoid `fib'''`).

```haskell
fib =
	\n ->
		case n of
			0 -> 0
			1 -> 1
			n' -> fib (n' - 1) + fib (n' - 2)
```

And if your function immediately does a pattern matching, like the `fib` above, you can do this:

```haskell
fib =
	\case
		0 -> 0
		1 -> 1
		n -> fib (n - 1) + fib (n - 2)
```

Easy, isn't it?

## Conclusion

"Haskell is an elegant language; therefore, it's a delightful choice for any coding." They say, from time to time. Well, they're lying, and they must have been top liars. Because the first half is correct - well, until you're actually working on a project in Haskell.

If you're here and you're still alive, welcome! This is really a big piece of cake on the topic of Haskell and functional programming.
