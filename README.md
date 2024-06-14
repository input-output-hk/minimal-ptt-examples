# Experimental minimal escrow contract repository

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
