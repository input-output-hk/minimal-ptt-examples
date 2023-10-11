# Writing Off-Chain code with Cooked Example

## Assumed Learning 



## Resources 

- [Minimal Escrow Example](https://github.com/Ali-Hill/minimal-ptt-examples/tree/escrow-cooked)
- [Cooked Repo](https://github.com/tweag/cooked-validators/)
- [Cooked Haddock](https://tweag.github.io/cooked-validators/)
- [Cheat sheet](https://github.com/tweag/cooked-validators/blob/main/doc/CHEATSHEET.md)
- [Example Cooked Contracts](https://github.com/tweag/cooked-smart-contracts)

## The Escrow Contract 


## Pay 

```haskell
-- | Pay some money into the escrow contract.
pay ::
    forall w s e.
    ( AsContractError e
    )
    => TypedValidator Escrow
    -- ^ The instance
    -> EscrowParams Datum
    -- ^ The escrow contract
    -> Value
    -- ^ How much money to pay in
    -> Contract w s e TxId
pay inst escrow vl = do
    pk <- ownFirstPaymentPubKeyHash
    let tx = Constraints.mustPayToTheScriptWithDatumInTx pk vl
          <> Constraints.mustValidateInTimeRange (Interval.interval 1 (1 + escrowDeadline escrow))
    mkTxConstraints (Constraints.typedValidatorLookups inst) tx
        >>= adjustUnbalancedTx
        >>= submitUnbalancedTx
        >>= return . getCardanoTxId
```

```haskell
pay ::
    MonadBlockChain m
    => Pl.TypedValidator Escrow
    -- ^ The instance
    -> Wallet
    -> EscrowParams Datum
    -- ^ The escrow contract
    -> Value
    -- ^ How much money to pay in
    -> m L.CardanoTx
pay inst submitter escrow vl = do
    let deadline = escrowDeadline escrow
    validityInterval <- slotRangeBefore (deadline - 1)
    (validateTxSkel $
     txSkelTemplate
      { txSkelOpts = def {txOptEnsureMinAda = True},
        txSkelSigners = [submitter],
        -- txSkelIns = [],
        txSkelOuts = [paysScript
                        inst
                        (walletPKHash submitter)
                        vl
                     ] ,
        txSkelValidityRange = validityInterval
      })
```
