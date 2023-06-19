# Prelude: Waking Up, Loaded

Prelude means an action or event serving as an introduction to something more important. Well, it basically means `#include <bits/stdc++.h>` for C++. We'll see how the word is used in clash later on.

## Architect. We really should start a new project.

Suppose the name of the repo is `my-clash-project`. Suppose you've got vscode and docker set up.

1. First, make sure you have `stack`.
2. Next, run `stack new my-clash-project clash-lang/simple`.
3. Then, copy  `.devcontainer/` directory from here to your fresh project root.
4. Finally, open `vscode` for out of box Haskell experience.
5. You don't need to but, if you're just too curious, read the `README.md` in your project root.

## Engineer. I want to add a new file.

It's actually called a `module`.

1. Go to `*.cabal` file under project root.
2. Search for `exposed-modules` under `library`.
3. Add the file, with or without `.hs`. Both are the same effect.

## Theorist. Documentations.

The portals to random clash documentations are placed here. If you want to figure things out by yourself like me, this is the ~~right~~ wrong place to come (too random at the moment).

[clash-lang/clash-compiler: Haskell to VHDL/Verilog/SystemVerilog compiler (github.com)](https://github.com/clash-lang/clash-compiler)

[Clash: Documentation (clash-lang.org)](https://clash-lang.org/documentation/)

[gergoerdi/clashilator: Generate interface between Clash and Verilator (github.com)](https://github.com/gergoerdi/clashilator)

[1. General — Clash Language 1.6.4 Manual (clash-lang.readthedocs.io)](https://clash-lang.readthedocs.io/en/v1.6.4/general/index.html)

[clash-prelude: Clash: a functional hardware description language - Prelude library (haskell.org)](https://hackage.haskell.org/package/clash-prelude-1.6.4)

[Clash.Prelude.Mealy (haskell.org)](https://hackage.haskell.org/package/clash-prelude-1.6.4/docs/Clash-Prelude-Mealy.html)

[Clash.Prelude.Moore (haskell.org)](https://hackage.haskell.org/package/clash-prelude-1.6.4/docs/Clash-Prelude-Moore.html)

[Clash.Signal (haskell.org)](https://hackage.haskell.org/package/clash-prelude-1.6.4/docs/Clash-Signal.html#v:unsafeFromReset)

[Clash.Promoted.Nat (haskell.org)](https://hackage.haskell.org/package/clash-prelude-1.6.4/docs/Clash-Promoted-Nat.html#t:SNat)

[GHC.TypeNats (haskell.org)](https://hackage.haskell.org/package/base-4.14.3.0/docs/GHC-TypeNats.html#t:KnownNat)

[Clash.Sized.Index (haskell.org)](https://hackage.haskell.org/package/clash-prelude-1.6.4/docs/Clash-Sized-Index.html#t:Index)

[Clash.Sized.Vector (haskell.org)](https://hackage.haskell.org/package/clash-prelude-1.6.4/docs/Clash-Sized-Vector.html#g:10)

[Clash.Sized.BitVector (haskell.org)](https://hackage.haskell.org/package/clash-prelude-1.6.4/docs/Clash-Sized-BitVector.html#t:BitVector)

[LighghtEeloo/clash-from-the-gates-up (github.com)](https://github.com/LighghtEeloo/clash-from-the-gates-up)

[LighghtEeloo/eecs470-clash (github.com)](https://github.com/LighghtEeloo/eecs470-clash)

[Lenses (fpcomplete.com)](https://www.fpcomplete.com/haskell/tutorial/lens/)

[ekmett/lens: Lenses, Folds, and Traversals - Join us on web.libera.chat #haskell-lens (github.com)](https://github.com/ekmett/lens#lens-lenses-folds-and-traversals)

[standardsemiconductor/lion: Where Lions Roam: RISC-V on the VELDT (github.com)](https://github.com/standardsemiconductor/lion)

[standardsemiconductor/riscv-formal at 645d49c5e4060c516a109575cb4193ff9e70d99b (github.com)](https://github.com/standardsemiconductor/riscv-formal/tree/645d49c5e4060c516a109575cb4193ff9e70d99b)

[mit-plv/riscv-semantics: A formal semantics of the RISC-V ISA in Haskell (github.com)](https://github.com/mit-plv/riscv-semantics/tree/master)

[riscv-semantics/src/Platform at master · mit-plv/riscv-semantics · GitHub](https://github.com/mit-plv/riscv-semantics/tree/master/src/Platform)

[ji-computer-architect/space-chip: A Major Design Experience (MDE) project in computer architecture for ECE4700J at Joint Institute. (github.com)](https://github.com/ji-computer-architect/space-chip)

[adamwalker/clash-utils: A collection of reusable Clash designs/examples (github.com)](https://github.com/adamwalker/clash-utils/tree/master)
