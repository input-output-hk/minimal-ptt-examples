# Experimental minimal escrow contract repository

## Current Build Instructions

1. go to the escrow folder 
2. run `nix develop .#escrow`
3. `cabal build escrow`

To enter testing repl after step 2 run:

 - `cabal repl escrow-test`

To run tests after step 2 run: 

- `cabal run escrow-test
