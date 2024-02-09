# Minimal Lotto Example 

Contract Model written for the lotto contract from Tweag's [cooked-smart-contracts](https://github.com/tweag/cooked-smart-contracts) repo. 

This example is interesting as it uses a contract model to check code that has been written in plutarch. The plutarch validators have been compiled to UPLC and

## Current Build Instructions

In the root folder: 

```
$ cd lotto/offchain
$ nix develop
$ cabal repl spec
$ import Test.QuickCheck
$ import Spec.Lotto
```

from here tests can be run such as:

```
$ quickCheck prop_Lotto
$ quickCheck prop_doubleSatisfaction
```

There are also additional tests defined in lotto/offchain/exe that can be run by:

```
$ cd lotto/offchain
$ nix develop
$ cabal run
```

## onchain building

Currently the plutus scripts from the onchain are already pre-compiled for convenience. If you want to recompile the onchain code then run: 

```
$ cd lotto/onchain
$ nix run .#lotto-onchain -- --ply
```

