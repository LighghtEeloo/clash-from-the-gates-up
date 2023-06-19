# Prelude: Waking Up, Loaded

Prelude means an action or event serving as an introduction to something more important. Well, it basically means `#include <bits/stdc++.h>` for C++. We'll see how it's used later on.

## Engineer. We really should start a new project.

Suppose the name of the repo is `my-clash-project`. Suppose you've got vscode and docker set up.

1. First, make sure you have `stack`.
2. Next, run `stack new my-clash-project clash-lang/simple`.
3. Then, copy  `.devcontainer/` directory from here to your fresh project root.
4. Finally, open `vscode` for out of box Haskell experience.

## Engineer. I want to add a new file.

It's actually called a `module`.

1. Go to `*.cabal` file under project root.
2. Search for `exposed-modules` under `library`.
3. Add the file, with or without `.hs`. Both are the same effect.

