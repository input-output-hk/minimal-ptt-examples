{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImportQualifiedPost  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:debug-context #-}
{-# OPTIONS_GHC -g -fplugin-opt PlutusTx.Plugin:coverage-all #-}
-- | A general-purpose escrow contract in Plutus
module Contract.OffChain where
import Cooked

import Control.Monad
import Cooked
import Data.Default
import qualified Data.Map as Map
import qualified Ledger.Tx as Tx
import qualified Plutus.Script.Utils.Ada as Ada
import qualified Plutus.Script.Utils.Typed as Pl
import Plutus.Script.Utils.Value
import Plutus.V2.Ledger.Api
import qualified Ledger as L
import qualified PlutusTx.Numeric as Pl
import PlutusTx.Numeric
import Prelude hiding ((-))
import qualified Plutus.V2.Ledger.Api as Pl
import Ledger.Tx.CardanoAPI qualified as CardanoAPI

import Debug.Trace

import Cardano.Node.Emulator.Params


import Control.Lens (review, view, has, only) -- makeClassyPrisms , view)
import Contract.Escrow

import Control.Monad.Except
import Plutus.Contract qualified as PC

-- Issue 1
{-
Could not deduce (PrettyCooked L.PaymentPubKeyHash)
        arising from a use of ‘paysScript’

used to be able to use this
-}


-- this should be fine once we define pay
{-
payEp ::
    forall w s e.
    ( PC.HasEndpoint "pay-escrow" Value s
    , AsEscrowError e
    )
    => EscrowParams Datum
    -> Wallet
    -> PC.Promise w s e L.CardanoTx -- Pl.TxId
payEp escrow w = PC.promiseMap
    (PC.mapError (review _EContractError))
    (PC.endpoint @"pay-escrow" $ pay (typedValidator escrow) w escrow)
-}

-- cooked-validators/src/Cooked/Skeleton.hs TxOpts
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

newtype RedeemSuccess = RedeemSuccess L.TxId
    deriving (Eq, Show)

--                  { txSkelOpts = def {txOptEnsureMinAda = True, txOptEmulatorParamsModification = Just $
--  EmulatorParamsModification increaseTransactionLimits},

redeem ::
    MonadBlockChain m
    => Pl.TypedValidator Escrow
    -> Wallet
--     -> Pl.TxOutRef
    -> EscrowParams Datum
    -> m RedeemSuccess
redeem inst submitter escrow = do
    outputs <-
      runUtxoSearch $
        utxosAtSearch (Pl.validatorAddress inst)
    current <- currentTime
    let
      uouts = map snd outputs
      deadline = escrowDeadline escrow
    validityInterval <- slotRangeBefore (deadline - 1)
    if (snd current) >= deadline
    then throwError $ FailWith "Deadline Passed"
    else if foldMap txOutValue uouts `lt` targetTotal escrow
      then throwError $ FailWith "Not enough funds at address"
      else do
        tx <- (validateTxSkel $
                txSkelTemplate
                  { txSkelOpts = def {txOptEnsureMinAda = True},
                    txSkelSigners = [submitter],
                    txSkelIns = Map.fromList (map (\ (or , _) -> ( or , TxSkelRedeemerForScript Redeem)) outputs),
                    txSkelOuts = map (\case
                                        PaymentPubKeyTarget pk vl -> paysPK (L.unPaymentPubKeyHash pk) vl
                                        ScriptTarget _ _ _ -> error "can't use script with cooked")
                                        (escrowTargets escrow)
                    ,
                    txSkelValidityRange = validityInterval
                  })
        return (RedeemSuccess (L.getCardanoTxId tx))

newtype RefundSuccess = RefundSuccess L.TxId
    deriving (Eq, Show)

refundFilter :: L.PaymentPubKeyHash -> [(TxOutRef , TxOut)] -> [(TxOutRef , TxOut)]
refundFilter pk sout =
    let
      flt (_ , ciTxOut) = (\case
                            NoOutputDatum	-> False
                            OutputDatumHash dh -> dh == L.datumHash (Datum (Pl.toBuiltinData pk))
                            OutputDatum d -> d == (Datum (Pl.toBuiltinData pk))) (txOutDatum ciTxOut)
    in
      (filter flt sout)


refund ::
    MonadBlockChain m
    => Pl.TypedValidator Escrow
    -> Wallet
    -> EscrowParams Datum
    -> m RefundSuccess
refund inst submitter escrow = do
    outputs <-
      runUtxoSearch $
        utxosAtSearch (Pl.validatorAddress inst)
    current <- currentTime
    let
      pk = walletPKHash submitter
      uouts = refundFilter (L.PaymentPubKeyHash pk) outputs
      deadline = escrowDeadline escrow
    validityInterval <- slotRangeAfter deadline
    tx <- (validateTxSkel $
            txSkelTemplate
              { txSkelOpts = def {txOptEnsureMinAda = True},
                txSkelSigners = [submitter],
                txSkelIns = Map.fromList (map (\ (or , _) -> ( or , TxSkelRedeemerForScript Refund)) uouts),
                txSkelValidityRange = validityInterval
              })
    if (snd current) <= escrowDeadline escrow
    then throwError $ FailWith "refund before deadline"
    else if uouts == []
    then throwError $ FailWith "no scripts to refund"
    else
      return (RefundSuccess (L.getCardanoTxId tx))


stealRefund ::
    MonadBlockChain m
    => Pl.TypedValidator Escrow
    -> Wallet
    -> Wallet
    -> EscrowParams Datum
    -> m L.CardanoTx
stealRefund inst submitter target escrow = do
    outputs <-
      runUtxoSearch $
        utxosAtSearch (Pl.validatorAddress inst)
    current <- currentTime
    let
      pk = walletPKHash target
      uouts = refundFilter (L.PaymentPubKeyHash pk) outputs
      deadline = escrowDeadline escrow
    validityInterval <- slotRangeAfter deadline
    tx <- (validateTxSkel $
            txSkelTemplate
              { txSkelOpts = def {txOptEnsureMinAda = True},
                txSkelSigners = [submitter],
                txSkelIns = Map.fromList (map (\ (or , _) -> ( or , TxSkelRedeemerForScript Refund)) uouts),
                txSkelValidityRange = validityInterval
              })
    if (snd current) <= escrowDeadline escrow
    then error "refund before deadline"
    else if uouts == []
    then error "no scripts to refund"
    else
      return tx
