# Have a Bit: Signed, Unsigned, Index, Bit, BitVector and BitPattern

With Clash data types we'll be able to describe the data that flows inside the wires and components we learned last time.

This session is meant to be unreadable by itself because it relies heavily on the documents of Clash; however, it's kinda meaningless to absorb information without motivation. So feel free to skip and find information later when needed.

## Architect. Let's use hackage for documentation.

Read [Clash.Sized.BitVector (haskell.org)](https://hackage.haskell.org/package/clash-prelude-1.6.4/docs/Clash-Sized-BitVector.html).

Learn how to browse the website for documentation. For all entries, see:

[clash-prelude: Clash: a functional hardware description language - Prelude library (haskell.org)](https://hackage.haskell.org/package/clash-prelude-1.6.4).

## Architect. A bunch of Clash data types.

The topics are listed in the title. It's now your job to figure out what they are.

## Engineer. Conversions.

If you know the conversion won't cause precision loss, basically, the size of wires shouldn't go down, then feel free to do it; if not, treat it with great care.

### `BitVector` to other data types

```haskell
bv :: (KnownNat n) => BitVector n

unpack . slice $ bv
```

### Other data types to `BitVector`

// Todo..

---

Feel free to advance to the [next session](4-data.md).
