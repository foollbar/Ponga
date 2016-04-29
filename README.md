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

this will generate a pair of trained gene in gene.txt.

there is no interface to amend training parameters yet, you may edit Train.hs directly.

## Visualize

```
stack exec visualize
```

you can observe the play of the generated genes.

## Play

```
stack exec play
```

try defeat them if you want.

use <kbd>i</kbd> to move up the paddle, <kbd>k</kbd> to move down.

## Simulation example

* Generation 10, 12 secs

![10 gene](https://cloud.githubusercontent.com/assets/1481772/14900008/8e17da34-0dc9-11e6-808e-5d5ae1c430a4.gif)


* Generation 20, 25 secs

![20 gene](https://cloud.githubusercontent.com/assets/1481772/14900007/8e16f07e-0dc9-11e6-9d37-06cfc4a688d5.gif)


* Generation 60, almost a minute long

![60 gene](https://cloud.githubusercontent.com/assets/1481772/14900006/8e16eb24-0dc9-11e6-86f8-0a17a1cf14f4.gif)
