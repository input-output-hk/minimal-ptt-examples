# Getting a DApp ready to use with the Plutus Testing Tool

# Table of Contents
- [Overview](#overview)
- [Prerequisites](#prerequisites)
- [Additional Nix Requirements](#nix)
- [Additional Cabal Requirements](#cabal)
- [IMPORTANT: How to depend on plutus-apps](#important)

## Overview <a name="overview"></a>

This document is a tutorial on how to get a DApp ready to be used with the Plutus Testing Tool. The Plutus Testing Tool will build a DApp and run the given certification object. This document only covers how to get an existing DApp with a certification object to work with the tool. For more information on how to write property besed tests and create a certification object see [Contract Model Tutorial](https://plutus-apps.readthedocs.io/en/latest/plutus/tutorials/contract-models.html).

Examples containing Dapps with certification objects that are compatible with the Plutus Testing Tool can be found in this [repo](https://github.com/Ali-Hill/minimal-ptt-examples).

## Prerequisites <a name="prerequisites"></a>

1. It is assumed that the DApp uses Nix flakes. 
2. The DApp needs to depend on [plutus-apps](https://github.com/input-output-hk/plutus-apps/).
3. The DApp needs to use a version of plutus-apps compatible with the `plutus-contract-certification` project from the v1.2.0 [release](https://github.com/input-output-hk/plutus-apps/tree/v1.2.0/plutus-contract-certification) of `plutus-apps`. 
4. It is assumed that the DApp is buildable on x86_64 linux architecture.

## Additional Nix Requirements <a name="nix"></a>

In order for the tool to build the project the nix flake needs to be exposed for the DApp. It is assumed that the flake is exposed to a variable called `iog.dapp`. 

### [IOGX](https://github.com/input-output-hk/iogx)

If you are using the IOGX flake template then you can expose the project by adding the following line to the `per-system-outputs.nix` file:

```nix
iog.dapp = projects.ghc8107;
```

An example repo using iogx can be found [here](https://github.com/Ali-Hill/minimal-ptt-examples/tree/escrow-iogx).

### Other Nix Setups

In all other nix setups you want to expose the project in your nix flake. In the [minimal examples repo](https://github.com/Ali-Hill/minimal-ptt-examples) the flake for the haskell project is called `rootProject`.  The project is then exposed in the minimal example by the adding following code to `flake.nix`:

```nix
iog.dapp = rootProject;
```

## Additional Cabal Requirements <a name="cabal"></a>

It is assumed that there is cabal project called `certification.cabal` that is exposed by the flake. The certification project should simply expose a module containing the certificaiton object called `certification` that is to be run on the Plutus Testing Tool. 

This is done for the escrow example using the following haskell code: 

```haskell
module Certification (certification) where

import Spec.Escrow (certification)
```

The `cabal` file for this simply exposes this certification module: 

```cabal 
library
  exposed-modules:  Certification
  build-depends:
    , base
    , plutus-contract-certification
    , escrow
```

## IMPORTANT: How to depend on plutus-apps <a name="important"></a>

The Plutus Testing Tool modifies the `cabal.project` file to remove the dependency on `plutus-contract-certification`. At the moment the `source-repository-package` definition needs to include the `--sha256` definition above the `subdir:` definition. The dependency on `plutus-contract-certification` also needs to go at the end of the `subdir:` list. An example of this is shown below: 

```cabal
source-repository-package
  type: git
  location: https://github.com/input-output-hk/plutus-apps.git
  tag: 5c54f209322f62b73d88150ecb5d781d515de746
  --sha256: krHvT653ngPWRaB0XVfupCkLUnohhmymjG6Vm9bawv0=
  subdir:
    cardano-node-emulator
    cardano-streaming
    freer-extras
    marconi-core
    marconi-chain-index
    pab-blockfrost
    plutus-chain-index
    plutus-chain-index-core
    plutus-contract
    plutus-pab
    plutus-ledger
    plutus-script-utils
    plutus-tx-constraints
    plutus-contract-certification
```    



