# Ponga

Pong game with Genetic AI, written in [Haskell](https://www.haskell.org/)

## Install

```
git clone https://github.com/foollbar/ponga.git
cd ponga
stack build
```

## Training

```
stack exec training
```

this will generate pair of trained gene in gene.txt.

there is no interface to amend parameters yet, you may edit Train.hs directly.

## Visualize

```
stack exec visualize
```

you can observe the play of generated genes.

## Play

```
stack exec play
```

try defeat them if you want.

use <kbd>i</kbd> to move up the paddle, <kbd>j</kbd> to move down.
