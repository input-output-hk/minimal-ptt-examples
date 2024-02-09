# Experimental minimal escrow contract repository

Additional repository for the Escrow contract with coverage disabled. This is because enabling coverage increases the UTXO size. This makes a difference to the Escrow contract so it is useful to test with this disabled. 

## Current Build Instructions

1. go to the escrow folder 
2. run `nix develop`
3. `cabal build escrow`

To enter testing repl after step 2 run in the escrow directory:

 - `cabal repl escrow-test`

To run tests after step 2 run in the escrow directory:

 - `cabal run escrow-test`
