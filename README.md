# Minimal governance contract repository

This is a repo containing a governance contract and contract model. 

This example is useful as it contains tests that observe the chain state. This is performed by the `checkLaw` DL test in the `tests/Spec/Governance.hs` file. This is run on a single example with `checkDL` and accross the whole model with `prop_checkLaw`.

## Build and running instructions

In the root folder of the repository: 

Enter a nix shell running the command: `nix develop`

Inside a nix-shell you can:

Build the repo with: 

- `cabal build governance`

Enter testing repl with:

 - `cabal repl gov-test`

Run tests with:

 - `cabal run gov-test`
