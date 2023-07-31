# Writing Contract Models Using Cooked Validators (rough)

## General Notes: 

- Wallets can be represented using integers and then changed to wallet using `(wallet int)`
- No need to define endpoints for contracts as `perform` is defined just on the offchain code 

## Don't need instance key boilerplate: 

```haskell
deriving instance Eq (ContractInstanceKey EscrowModel w s e params)
deriving instance Show (ContractInstanceKey EscrowModel w s e params)

data ContractInstanceKey EscrowModel w s e params where
  WalletKey :: Wallet -> ContractInstanceKey EscrowModel () EscrowTestSchema EscrowError ()`
```

## NextState definition is exactly the same however the `withdraw` and `deposit` functions take in a wallet address instead of a wallet

This cardano address can be obtained from a wallet by: `walletAddr $ wallet w`

## Definitions of `preconditions` and `abitrary actions` are the same however the `perform` and `monitoring` functions are written as an instance of `RunModel` for the `SuperMockChain` as follows: 

```haskell
instance RunModel EscrowModel (SuperMockChain ()) where

    perform _ (action parameters) _ = 
    perform _ (action2 parameters) _ = 
    ... 
```

## How to define default proposition?

This is now defined using an initial distribution which contains a list of wallets and their values:

```haskell
-- | initial distribution wher everyone has 20 ada
testInit :: InitialDistribution
testInit = initialDistribution [(i, [Ada.lovelaceValueOf 20_000_000]) | i <- knownWallets]
```

The default contract model proposition that tests that all balance changes match the model is defined as follows:

```haskell
prop_Escrow :: Actions EscrowModel -> Property
prop_Escrow = propRunActions testInit () balanceChangePredicate
```

