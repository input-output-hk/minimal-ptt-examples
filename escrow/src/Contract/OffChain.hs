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

{-
import Control.Lens (review, view) -- makeClassyPrisms , view)
import Control.Monad
import Cooked.MockChain
-- import Cooked.Tx.Constraints
import qualified Ledger as Pl
-- import qualified Ledger.Ada as Pl

import qualified Ledger.Typed.Scripts as Pl
import Plutus.Script.Utils.Value (Value, geq, lt)

-- import Ledger.Value (Value, lt) -- geq , lt)
import Prelude
import qualified Ledger as L
import Plutus.Contract qualified as PC
import Ledger.Tx qualified as Tx
import PlutusTx qualified
-- import Plutus.Script.Utils.V1.Scripts (datumHash)
import Data.Default
import Plutus.V1.Ledger.Scripts (Datum (Datum)) -- DatumHash, ValidatorHash)
import Cooked.Wallet
-}

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

import Control.Lens (review, view) -- makeClassyPrisms , view)


-- import qualified Ledger.Typed.Scripts as Pl
-- import Control.Lens (review, view) -- makeClassyPrisms , view)
-- import Plutus.Contract qualified as PC

import Contract.Escrow

{-
escrowContract :: MonadBlockChain m => Pl.TypedValidator Escrow -> EscrowParams Datum -> m ()
escrowContract v escrow =
    let inst = typedValidator escrow
        payAndRefund = endpoint @"pay-escrow" $ \vl -> do
            _ <- pay inst escrow vl
            _ <- awaitTime $ escrowDeadline escrow
            refund inst escrow
    in selectList
        [ void payAndRefund
        , void $ redeemEp escrow
        ]
-}

-- Issue 1
{-
Could not deduce (PrettyCooked L.PaymentPubKeyHash)
        arising from a use of ‘paysScript’

used to be able to use this
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

redeem ::
    MonadBlockChain m
    => Pl.TypedValidator Escrow
    -> Wallet
    -> Pl.TxOutRef
    -> EscrowParams Datum
    -> m RedeemSuccess
redeem inst submitter oref escrow = do
    [(oref, output)] <-
      runUtxoSearch $
        utxosAtSearch (Pl.validatorAddress inst)
    easy <-
      runUtxoSearch $
        utxosAtSearch (Pl.validatorAddress inst)
    -- unspentOutputs <- scriptUtxosSuchThat inst (\_ _ -> True)
    current <- currentTime -- FIX
    let
      uouts = map snd easy
      deadline = escrowDeadline escrow
    validityInterval <- slotRangeAfter deadline
    if (fst current) >= deadline
    then error "Deadline Passed"
--    else if foldMap (view Tx.ciTxOutValue) uouts `lt` targetTotal escrow
    else if foldMap txOutValue uouts `lt` targetTotal escrow
      then error "Not enough funds at address"
      else do
        tx <- (validateTxSkel $
                txSkelTemplate
                  { txSkelOpts = def {txOptEnsureMinAda = True},
                    txSkelSigners = [submitter],
                    -- txSkelIns = [map (SpendsScript inst Redeem . fst) unspentOutputs],
                    txSkelIns = Map.singleton oref $ TxSkelRedeemerForScript Redeem,
                    txSkelOuts = map (\case
                                        PaymentPubKeyTarget pk vl -> paysPK (L.unPaymentPubKeyHash pk) vl
                                        ScriptTarget _ _ _ -> error "can't use script with cooked")
                                        (escrowTargets escrow)
                    ,
                    txSkelValidityRange = validityInterval
                  })
        return (RedeemSuccess (L.getCardanoTxId tx))


{-
redeem ::
    MonadBlockChain m
    => Pl.TypedValidator Escrow
    -> EscrowParams Datum
    -> m RedeemSuccess
redeem inst escrow = do
    unspentOutputs <- scriptUtxosSuchThat inst (\_ _ -> True)
    current <- currentTime
    let
      uouts = map snd (map fst unspentOutputs)
      deadline = L.interval 1 (escrowDeadline escrow)
    if current >= escrowDeadline escrow
    then error "Deadline Passed"
--    else if foldMap (view Tx.ciTxOutValue) uouts `lt` targetTotal escrow
    else if foldMap Tx.decoratedTxOutPlutusValue uouts `lt` targetTotal escrow
      then error "Not enough funds at address"
      else do
        tx <-
          validateTxSkel $
              -- txSkelOpts (def {adjustUnbalTx = True}) $
                  (ValidateIn deadline
                    : map (SpendsScript inst Redeem . fst) unspentOutputs)
                      :=>: map (\case
                                  PaymentPubKeyTarget pk vl -> paysPK (L.unPaymentPubKeyHash pk) vl
                                  ScriptTarget _ _ _ -> error "can't use script with cooked")
                                  (escrowTargets escrow)
        return (RedeemSuccess (L.getCardanoTxId tx))

newtype RefundSuccess = RefundSuccess L.TxId
    deriving (Eq, Show)
-}

{-
-- | 'refund' with an endpoint.
refundEp ::
    forall w s.
    ( PC.HasEndpoint "refund-escrow" () s
    )
    => EscrowParams Datum
    -> PC.Promise w s EscrowError RefundSuccess
refundEp escrow = PC.endpoint @"refund-escrow" $ \() -> refund (typedValidator escrow) escrow
-}

{-
refundFilter :: L.PaymentPubKeyHash -> [SpendableOut] -> [SpendableOut]
refundFilter pk sout =
    let
      flt (_ , ciTxOut) = either id datumHash (Tx._ciTxOutDatum ciTxOut) == datumHash (Datum (PlutusTx.toBuiltinData pk))
    in
      (filter flt sout)

refund ::
    MonadBlockChain m
    => Pl.TypedValidator Escrow
    -> EscrowParams Datum
    -> m RefundSuccess
refund inst escrow = do
    pk <- ownPaymentPubKeyHash
    unspentOutputs <- scriptUtxosSuchThat inst (\_ _ -> True)
    current <- currentTime
    let
      uouts = refundFilter (L.PaymentPubKeyHash pk) (map fst unspentOutputs)
    tx <- validateTxSkel $
              -- txSkelOpts (def {adjustUnbalTx = True}) $
                  (After (escrowDeadline escrow)
                    : map (SpendsScript inst Refund) uouts)
    if current <= escrowDeadline escrow
    then error "refund before deadline"
    else if (map (SpendsScript inst Refund) uouts) == []
    then error "no scripts to refund"
    else
      return (RefundSuccess (L.getCardanoTxId tx))
-}

-- Todo define payRedeemRefund

{-
txRefund :: MonadBlockChain m => m ()
txRefund = do
  funder <- ownPaymentPubKeyHash
  utxos <-
    scriptUtxosSuchThat Cf.crowdfundingValidator (\d _ -> Cf.getFunder d == Just funder)
  void $
    validateTxSkel $
      txSkel $
        map (SpendsScript Cf.crowdfundingValidator Cf.IndividualRefund . fst) utxos
          :=>: [paysPK funder (PlutusTx.Prelude.sum $ map (sOutValue . fst) utxos)]
-}
