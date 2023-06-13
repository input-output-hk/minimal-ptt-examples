{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Spec.EscrowSpec where

import Contract.Escrow
import Contract.OffChain

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
import Prelude hiding ((-), (+))
import qualified Plutus.V2.Ledger.Api as Pl
import Ledger.Tx.CardanoAPI qualified as CardanoAPI

import Cooked.Wallet
import Test.Tasty
-- import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit
import Test.Tasty.Runners (TestTree(TestGroup))

import Cardano.Node.Emulator.TimeSlot qualified as TimeSlot



import Test.QuickCheck
import Test.QuickCheck.ContractModel hiding (awaitSlot)
import Test.QuickCheck.ContractModel.Cooked
import Test.QuickCheck.ContractModel.ThreatModel
import Test.QuickCheck.ContractModel.ThreatModel.DoubleSatisfaction
import Test.Tasty
import Test.Tasty.QuickCheck


-- | initial distribution s.t. everyone owns five bananas
testInit :: InitialDistribution
testInit = initialDistribution [(i, [Ada.lovelaceValueOf 20_000_000]) | i <- knownWallets]

escrowParams :: POSIXTime -> EscrowParams d
escrowParams startTime =
  EscrowParams
    { escrowDeadline = startTime + 40000
    , escrowTargets  =
        [ payToPaymentPubKeyTarget (L.PaymentPubKeyHash (walletPKHash $ wallet 1)) (Ada.adaValueOf 10)
        , payToPaymentPubKeyTarget (L.PaymentPubKeyHash (walletPKHash $ wallet 2)) (Ada.adaValueOf 20)
        ]
    }

tests :: TestTree
tests =
  testGroup
    "EscrowSpec"
      [ testCase "Simple example succeeds" usageExample,
        testCase "Can redeem"
                $ testSucceedsFrom def testInit redeemTrace ,
        testCase "can redeem even if more money than required has been paid in"
                $ testSucceedsFrom def testInit redeem2Trace,
        testCase "Can refund"
                $ testSucceedsFrom def testInit refundTrace,
        testCase "Wallet receives refund"
                $ testSucceedsFrom def testInit refundCheck,
        testCase "Wallet receives redeem"
                $ testSucceedsFrom def testInit redeemCheck]

usageExample :: Assertion
usageExample = testSucceedsFrom def testInit $ do
    pay (typedValidator (escrowParams (TimeSlot.scSlotZeroTime def)))
        (wallet 1)
        (escrowParams (TimeSlot.scSlotZeroTime def))
        (Ada.adaValueOf 30)

redeemTrace :: MonadBlockChain m => m RedeemSuccess
redeemTrace = do
    let
        val = (typedValidator (escrowParams (TimeSlot.scSlotZeroTime def)))
        params = (escrowParams (TimeSlot.scSlotZeroTime def))
    pay val (wallet 1) params (Ada.adaValueOf 20)
    pay val (wallet 2) params (Ada.adaValueOf 10)
    redeem val (wallet 3) params

redeem2Trace :: MonadBlockChain m => m RedeemSuccess
redeem2Trace = do
    let
        val = (typedValidator (escrowParams (TimeSlot.scSlotZeroTime def)))
        params = (escrowParams (TimeSlot.scSlotZeroTime def))
    pay val (wallet 1) params (Ada.adaValueOf 20)
    pay val (wallet 2) params (Ada.adaValueOf 10)
    pay val (wallet 3) params (Ada.adaValueOf 10)
    redeem val (wallet 1) params

refundTrace :: MonadBlockChain m => m RefundSuccess
refundTrace = do
    (_ , t0) <- currentTime
    let
        val = (typedValidator (escrowParams (TimeSlot.scSlotZeroTime def)))
        params = (escrowParams (TimeSlot.scSlotZeroTime def))
        deadline = t0 + 60_000
    pay val (wallet 1) params (Ada.adaValueOf 20)
    deadlineSlot <- getEnclosingSlot deadline
    void $ awaitSlot $ deadlineSlot + 1
    refund val (wallet 1) params

payWallet ::
    MonadBlockChain m
    => Wallet
--     -> Pl.TxOutRef
    -> Wallet
    -> Value
    -> m L.CardanoTx
payWallet submitter target v = do
        validateTxSkel $
                txSkelTemplate
                  { txSkelOpts = def {txOptEnsureMinAda = True},
                    txSkelSigners = [submitter],
                    txSkelOuts = [paysPK (walletPKHash target) v]
                  }


escrowParams' :: POSIXTime -> EscrowParams d
escrowParams' startTime =
  EscrowParams
    { escrowDeadline = startTime + 40000
    , escrowTargets  =
        [ payToPaymentPubKeyTarget (L.PaymentPubKeyHash (walletPKHash $ wallet 1)) (Ada.adaValueOf 100)
        ]
    }

refundCheck :: MonadBlockChain m => m L.CardanoTx
refundCheck = do
    (_ , t0) <- currentTime
    let
        val = (typedValidator (escrowParams' (TimeSlot.scSlotZeroTime def)))
        params = (escrowParams' (TimeSlot.scSlotZeroTime def))
        deadline = t0 + 60_000
    pay val (wallet 1) params (Ada.adaValueOf 100)
    deadlineSlot <- getEnclosingSlot deadline
    void $ awaitSlot $ deadlineSlot + 1
    refund val (wallet 1) params
    payWallet (wallet 1) (wallet 2) (Ada.adaValueOf 920)

redeemCheck :: MonadBlockChain m => m L.CardanoTx
redeemCheck = do
    (_ , t0) <- currentTime
    let
        val = (typedValidator (escrowParams' (TimeSlot.scSlotZeroTime def)))
        params = (escrowParams' (TimeSlot.scSlotZeroTime def))
        deadline = t0 + 60_000
    pay val (wallet 1) params (Ada.adaValueOf 100)
    deadlineSlot <- getEnclosingSlot deadline
    redeem val (wallet 1) params
    void $ awaitSlot $ deadlineSlot + 1
    payWallet (wallet 1) (wallet 2) (Ada.adaValueOf 920)
