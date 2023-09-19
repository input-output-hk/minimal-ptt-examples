# Creating a Certification Object 

# Table of Contents
- [Overview](#overview)
- [Certification Object](#intro)
  - [Certification Properties](#properties)
  - [Defining a Certification Object](#definition)

## Overview <a name="overview"></a>

This document details how to create a certification object for a DApp that is used to run tests on the Plutus Testing Tool. This document assumes that a contract model is already defined for the given Dapp. For more information on how to define a contract model follow this [Contract Model Tutorial](https://plutus-apps.readthedocs.io/en/latest/plutus/tutorials/contract-models.html). 
 
Examples containing Dapps with certification objects that are compatible with the Plutus Testing Tool can be found in this [repo](https://github.com/Ali-Hill/minimal-ptt-examples).

## Certification Object <a name="intro"></a>

The certification object is a standardised way to define a list of tests to run using the defined contract model. This includes all of the tests defined in the [Contract Model Tutorial](https://plutus-apps.readthedocs.io/en/latest/plutus/tutorials/contract-models.html). The certification object is defined by the following code:


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

### Certification Properties <a name="properties"></a>

**Standard Property Test**

The standard property test runs the model against an emulator to check to see if there are any discrepencies between the model and emulator. 

**No Locked Funds**

This property tests that funds locked in the contract can be retrieved by any wallet without cooperation from any other wallets. 

**No Locked Funds Light**

This property tests that funds locked in the contract can be retrieved but allows for cooperation between wallets. 

**Crash Tolerance**

This property tests to see if a contract instance can be restarted in the event of a crash and whether once restarted the contract still behaves as expected by the model. 

**Double Satisfaction**

This property tests to see if a conctract is vulnerable to the [double satisfaction vulnerability](https://plutus.readthedocs.io/en/latest/reference/writing-scripts/common-weaknesses/double-satisfaction.html). 

**Other Tests**

Other tests such as unit tests are run using a [Tasty](https://hackage.haskell.org/package/tasty-1.5/docs/Test-Tasty.html) test tree.

**DL Tests**

DL tests are a form of unit tests using [Dynamic Logic](https://hackage.haskell.org/package/quickcheck-dynamic-3.3.1/docs/Test-QuickCheck-DynamicLogic.html) that run traces actions on the defined model. 

### Defining a Certification Object <a name="definition"></a>

**Remark on Properties**

You may notice that there are more properties than there are fields in the certification object. This is because a certification will automatically run the standard property test and double satisfaction test by default. At the moment double satisfaction is temporarily disabled.

The certification object for the escrow contract model defined in the [Contract Model Tutorial](https://plutus-apps.readthedocs.io/en/latest/plutus/tutorials/contract-models.html) is defined by: 

```haskell
certification :: Certification EscrowModel
certification = defaultCertification {
    certCoverageIndex      = covIdx, 
    certNoLockedFunds = Just noLockProof,
    certCrashTolerance = Just Instance,
    certUnitTests = Just unitTest,
    certDLTests = [("redeem test", unitTest1), ("refund test", unitTest2)],
  }
  where unitTest _ = tests
```

The following explanations refer to the Escrow [Contract Model Tutorial](https://plutus-apps.readthedocs.io/en/latest/plutus/tutorials/contract-models.html):

``certCoverageIndex``

This is defined by giving a `CoverageIndex` for the validators.

``certNoLockedFunds``

This is defined by giving a `noLockProof`.

``certCrashTolerance``

This is always defined by supplying `just Instance` with the assumption that an instance of `CrashTolerance` for the model is defined in scope. 

``certUnitTests``

This is defined by supplying a `Tasty` test tree. Note that the above definition `unitTest _ = tests` is defined to take into account the `coverageRef` argument.

``certDLTests``

This is defined by giving a list of pairs of `DL` tests and strings indicating their names. A DL test is a unit test defined using the model. For more information about DL tests see [Dynamic Logic Tests](https://hackage.haskell.org/package/quickcheck-dynamic-3.3.1/docs/Test-QuickCheck-DynamicLogic.html). An example DL test for the Escrow contract is given by:

```haskell
refundTest :: DL EscrowModel ()
refundTest = do
               val <- forAllQ $ chooseQ (10, 20)
               action $ Pay w1 val
               waitUntilDL 100
               action $ Refund w1
```




