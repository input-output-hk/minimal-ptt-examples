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
                $ testSucceedsFrom def testInit redeemTrace,
        testCase "can redeem even if more money than required has been paid in"
                $ testSucceedsFrom def testInit redeem2Trace]

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

{--- typedValidator

redeem2Trace :: MonadMockChain m => m RedeemSuccess
redeem2Trace = do
    let
        val = (typedValidator (escrowParams (TimeSlot.scSlotZeroTime def)))
        params = (escrowParams (TimeSlot.scSlotZeroTime def))
    pay val params (Ada.adaValueOf 20) `as` wallet 1
    pay val params (Ada.adaValueOf 10) `as` wallet 2
    pay val params (Ada.adaValueOf 10) `as` wallet 3
    redeem val params `as` wallet 1

{-
refundTrace :: MonadMockChain m => m RefundSuccess
refundTrace = do
    t0 <- currentTime
    let
        val = (typedValidator (escrowParams (TimeSlot.scSlotZeroTime def)))
        params = (escrowParams (TimeSlot.scSlotZeroTime def))
        deadline = t0 + 60_000
    pay val params (Ada.adaValueOf 20) `as` wallet 1
    awaitTime deadline
    refund val params `as` wallet 1
-}

-- | helper function to compute what the given wallet owns in the
-- given state
holdingInState :: UtxoState -> Wallet -> L.Value
holdingInState (UtxoState m) w
  | Just vs <- M.lookup (walletAddress w) m = utxoValueSetTotal vs
  | otherwise = mempty

holdingInState2 :: InitialDistribution -> Wallet -> L.Value
holdingInState2 d w = mconcat (valuesForWallet d w)

-- Terrible way to do this but just using this to compare values assuming some fee has been applied
naiveValueComparison :: L.Value -> L.Value -> Bool
naiveValueComparison v1 v2 = if (Ada.fromValue v1) >= (Ada.fromValue v2)
                        && (floor $ (* 0.999) $ fromIntegral (Ada.getLovelace (Ada.fromValue v1)))
                                <= (Ada.getLovelace (Ada.fromValue v2))
                                        then True else False

tests :: TestTree
tests =
    testGroup
        "EscrowSpec"
            [ testCase "Simple example succeeds" usageExample,
              testCase "Can redeem"
                $ testSucceeds
                    (allowBigTransactions redeemTrace),
              testCase "Check wallets and can redeem"
                $ testSucceedsFrom'
                    ( \_ s ->
                       -- testBool $ (Ada.fromValue ((holdingInState2 testInit (wallet 2)) <> (Ada.adaValueOf 10))
                         --           == Ada.fromValue (holdingInState s (wallet 2)))
                        -- testBool $ (Ada.fromValue ((holdingInState2 testInit (wallet 3)))
                         --           == Ada.fromValue (holdingInState s (wallet 3)))
                        testBool $ naiveValueComparison
                                        ((holdingInState2 testInit (wallet 2))
                                                <> (Ada.adaValueOf 10))
                                        (holdingInState s (wallet 2))
                        .&&. (testBool $ naiveValueComparison
                                           ((holdingInState2 testInit (wallet 1))
                                             <> (Ada.adaValueOf (-10)))
                                           (holdingInState s (wallet 1)))
                        -- .&&. (testBool $ naiveValueComparison
                        --                   (holdingInState2 testInit (wallet 3))
                        --                   (holdingInState s (wallet 3)))

                    )
                    testInit
                    (allowBigTransactions redeemTrace),
              testCase "can redeem even if more money than required has been paid in"
                $ testSucceedsFrom'
                    ( \_ s ->
                        testBool $ naiveValueComparison
                                           ((holdingInState2 testInit (wallet 2))
                                             <> (Ada.adaValueOf 10))
                                           (holdingInState s (wallet 2))
                        .&&. (testBool $ naiveValueComparison
                                           ((holdingInState2 testInit (wallet 3))
                                             <> (Ada.adaValueOf (-10)))
                                           (holdingInState s (wallet 3)))
                    )
                    testInit
                    (allowBigTransactions redeem2Trace)-- ,
              -- testCase "Can refund"
              --   $ testSucceeds
              --      (allowBigTransactions refundTrace)
            ]
-}
