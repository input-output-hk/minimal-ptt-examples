# Minimal Plutus Testing Examples Repository

## Overview 

This repo contains a variety of minimal examples showcasing the testing of DApps written in plutus using the quickcheck contractmodel library. This contains examples using a variety of techniques and emulators. The emulators include the plutus-apps emulator, cooked emulator and the cardano-node-emulator. Techniques include model-based property testing, threat modelling such as double satisfaction as well as negative testing. Each example is contained in a separate branch and written with a minimal configuration so that it can be taken and adapted for other developers. 

## Examples 

The notable branches are listed below by emulator: 

The main branch includes the `cardano-node-emulator` version of the escrow contract as it is the most up-to-date example. 

*cardano-node-emulator*

1. escrow-node-emulator
2. vesting-node-emulator

*cooked emuator*

1. escrow-cooked
2. minimal-lotto-experimental
3. auction-cooked

*plutus-apps emulator*

1. Escrow
2. EscrowNoCoverage
3. Vesting 
4. governance

## Current Build Instructions

1. run `nix develop`
2. `cabal build escrow`

To enter testing repl after step 2 run in the root directory:

 - `cabal repl escrow-test`

To run tests after step 2 run in the root directory:

 - `cabal run escrow-test`

If you want to run a specific test such as Double Satisfaction in the repl do:

- `import Test.QuickCheck`
- `import Spec.Escrow`
- `quickCheck prop_Escrow_DoubleSatisfaction`
