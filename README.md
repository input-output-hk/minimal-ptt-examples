# Experimental minimal escrow contract repository

Escrow contract model example running on the plutus-apps emulator.

## Current Build Instructions

1. go to the escrow folder 
2. run `nix develop`
3. `cabal build escrow`

To enter testing repl after step 2 run in the escrow directory:

 - `cabal repl escrow-test`

To run tests after step 2 run in the escrow directory:

 - `cabal run escrow-test`
