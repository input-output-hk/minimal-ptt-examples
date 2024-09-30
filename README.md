# Minimal Plutus Testing Examples Repository

## Overview 

This repo contains an Escrow contract running on the cardano node emulator. The `Refund` endpoint intentionally does not correctly reflect the correct balance in the model. The correct line is commented out in the `nextState` function. This has been done for users to see what an error would look like when testing the model. 

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
