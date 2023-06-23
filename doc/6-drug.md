# Gateway Drug #2: Moore and Mealy

In a few lines shall you see the power of circuit gods.

## Architect. We can get rid of all naked `register`s.

Though sometimes we can use registers, it's not healthy to deal with them all the time. We need better abstractions.

This is an exam for you to prove that you are capable of writing basic hardware description code in Clash.  The exam is short and simple:

1. Search for `moore` and `mealy`. Use documentation if necessary. If you've known what they are, good.
2. Implement `moore'` and `mealy'` by yourself.

```haskell
moore' ::
  (HiddenClockResetEnable dom, NFDataX s, NFDataX i, NFDataX o) =>
  (s -> i -> s) ->
  (s -> o) ->
  s ->
  (Signal dom i -> Signal dom o)


mealy' ::
  (HiddenClockResetEnable dom, NFDataX s, NFDataX i, NFDataX o) =>
  (s -> i -> (s, o)) ->
  s ->
  (Signal dom i -> Signal dom o)


```

3. Think about whether `moore` and `mealy` can be used to represent each other.

## Engineer. I've got a reference answer.

Check out [src/Ignite/Project.hs](../src/Ignite/Project.hs). No cheating! (Just kidding.)
