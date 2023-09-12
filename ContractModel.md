# Creating a Certification Object 

# Table of Contents
- [Overview](#overview)
- [Certification Object Introduction](#intro)
- [Additional Nix Requirements](#nix)
- [Additional Cabal Requirements](#cabal)
- [IMPORTANT: How to depend on plutus-apps](#important)

## Overview <a name="overview"></a>

This document details how to create a certification object for a DApp that is used to run tests on the Plutus Testing Tool. This document assumes that a contract model is already defined for the given Dapp. For more information on how to define a contract model follow this [Contract Model Tutorial](https://plutus-apps.readthedocs.io/en/latest/plutus/tutorials/contract-models.html). 
 
Examples containing Dapps with certification objects that are compatible with the Plutus Testing Tool can be found in this [repo](https://github.com/Ali-Hill/minimal-ptt-examples).

## Certification Object Introduction <a name="intro"></a>

The certification object is a way to define a list of tests to run using the defined contract model. This includes all of the tests defined in the [Contract Model Tutorial](https://plutus-apps.readthedocs.io/en/latest/plutus/tutorials/contract-models.html). The certification object is defined by the following code:


```haskell
data Certification m = Certification {
    certCoverageIndex      :: CoverageIndex,                      -- ^ Coverage locations for on-chain test coverage.
    certNoLockedFunds      :: Maybe (NoLockedFundsProof m),
    certNoLockedFundsLight :: Maybe (NoLockedFundsProofLight m),
    certCrashTolerance     :: Maybe (Instance CrashTolerance m),  -- ^ Contract model for testing robustness against off-chain code crashes.
    certWhitelist          :: Maybe Whitelist,                    -- ^ List of allowed exceptions from on-chain code. Usually `Just 'defaultWhiteList'`.
    certUnitTests          :: Maybe (CoverageRef -> TestTree),    -- ^ Unit tests using "Test.Tasty". See e.g. 'Plutus.Contract.Test.checkPredicateCoverage'.
    certDLTests            :: [(String, DL m ())]                 -- ^ Unit tests using 'Plutus.Contract.Test.ContractModel.DL'.
  }
```

The plutus testing tool will take a certification object and run it using the `defaultCertification` options. This certification object is used to test a variety of properties: 

### Certification Properties

**Standard Property Test**

The standard property test runs the model against an emulator to check to see if there are any discrepencies between the model and emulator. 

**No Locked Funds**

This property tests that funds locked in the contract can be retrieved by any wallet without cooperation from any other wallets. 

**No Locked Funds Light**

This property tests that funds locked in the contract can be retrieved but allows for cooperation between wallets. 

**Crash Tolerance**

This property tests to see if a contract instance can be restarted in the event of a crash and whether once restarted the contract still behaves as expected by the model. 

**Double Satisfaction**

This property tests to see if a conctract is vulnerable to the [double satisfaction](https://plutus.readthedocs.io/en/latest/reference/writing-scripts/common-weaknesses/double-satisfaction.html). 

**Unit Tests**

Unit tests run a *Tasty* test tree containing arbitrary unit tests. 

**DL Tests**

DL tests are a form of unit tests that run specific traces of actions on the defined model. 

### 

By default all certifications will run the model itself with coverage as well as check for double satisfaction (this has been disabled temporarily). The certification object for the escrow contract model defined in the [Contract Model Tutorial](https://plutus-apps.readthedocs.io/en/latest/plutus/tutorials/contract-models.html) is defined by: 

```haskell
certification :: Certification EscrowModel
certification = defaultCertification {
    certNoLockedFunds = Just noLockProof,
    certCrashTolerance = Just Instance,
    certUnitTests = Just unitTest,
    certDLTests = [("redeem test", unitTest1), ("refund test", unitTest2)],
    certCoverageIndex      = covIdx
  }
  where unitTest _ = tests
```

The coverage index 
 


Note that `tests` is an arbitrary `Tasty` test tree. 

