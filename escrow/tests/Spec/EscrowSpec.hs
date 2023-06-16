{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Spec.EscrowSpec where

-- Imports Copied From Auction Example
import Control.Lens hiding (elements)
import Control.Monad.Reader

import Data.Default
import Data.Fixed (Micro)
import Data.Maybe

import GHC.Generics hiding (to)

-- import Cardano.Api
import Cardano.Node.Emulator.TimeSlot qualified as Plutus

import Ledger.Tx.CardanoAPI qualified as Plutus

import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.V1.Ledger.Value qualified as Plutus hiding (adaSymbol, adaToken)

-- import Test.QuickCheck
import Test.QuickCheck qualified as QC
import Test.QuickCheck.ContractModel hiding (inv)
import Test.QuickCheck.ContractModel.Cooked
import Test.QuickCheck.ContractModel.ThreatModel
import Test.QuickCheck.ContractModel.ThreatModel.DoubleSatisfaction
import Test.Tasty
import Test.Tasty.QuickCheck hiding (scale)

import Cooked.Currencies
import Cooked.Wallet

-- Imports Added by Me
import Contract.OffChain
import Contract.Escrow
import qualified Ledger as L
import Data.Map (Map)
import qualified Data.Map as Map
import Cardano.Node.Emulator.TimeSlot qualified as TimeSlot
import Cooked hiding (currentSlot)
import Test.Tasty.HUnit
import Cardano.Api qualified as Api
import Plutus.V1.Ledger.Value hiding (adaSymbol, adaToken)
import Data.Foldable

import Ledger.CardanoWallet
import Test.QuickCheck.StateModel.Variables
-- import PlutusTx.Prelude qualified as Pre
-- deriving instance Generic Builtin.BuiltinByteString
import Test.QuickCheck.StateModel.Variables

import PlutusTx.Builtins.Internal qualified as Builtin

-- | initial distribution s.t. everyone owns five bananas
testInit :: InitialDistribution
testInit = initialDistribution [(i, [Ada.lovelaceValueOf 20_000_000]) | i <- knownWallets]

data EscrowModel = EscrowModel { _contributions :: Map Int Integer
                               , _refundSlot    :: L.Slot
                               , _targets       :: Map Int Integer
                               } deriving (Eq, Show, Generic)

makeLenses 'EscrowModel

modelParams :: EscrowParams d
modelParams = escrowParams $ TimeSlot.scSlotZeroTime def

instance ContractModel EscrowModel where
  data Action EscrowModel = Pay Int Integer
                          | Redeem Int
                          | Refund Int
                          | BadRefund Int Int
                          deriving (Eq, Show, Generic)


  initialState = EscrowModel { _contributions = Map.empty
                             , _refundSlot    = TimeSlot.posixTimeToEnclosingSlot def
                                              . escrowDeadline
                                              $ modelParams
                             -- TODO: This model is somewhat limited because we focus on one
                             -- set of parameters only. The solution is to use the sealed bid
                             -- auction trick to generate parameters dynamically that we can
                             -- use later on.
                             , _targets       = Map.fromList [ (1, 10)
                                                             , (2, 20)
                                                             ]
                             }

  nextState a = void $ case a of
    Pay w v -> do
      withdraw (walletAddr $ wallet w) (Ada.adaValueOf $ fromInteger v)
      contributions %= Map.insertWith (+) w v
      wait 1
    Redeem w -> do
      targets <- viewContractState targets
      contribs <- viewContractState contributions
      sequence_ [ deposit (walletAddr (wallet w)) (Ada.adaValueOf $ fromInteger v) | (w, v) <- Map.toList targets ]
      let leftoverValue = sum contribs - sum targets
      deposit (walletAddr $ wallet w) (Ada.adaValueOf $ fromInteger leftoverValue)
      contributions .= Map.empty
      wait 1
    Refund w -> do
      v <- viewContractState $ contributions . at w . to sum -- to fold
      contributions %= Map.delete w
      deposit (walletAddr $ wallet w) (Ada.adaValueOf $ fromInteger v)
      wait 1
    -- BadRefund _ _ -> do
      -- wait 2

  precondition s a = case a of
    Redeem _ -> (s ^. contractState . contributions . to sum) >= (s ^. contractState . targets . to sum)
             && (s ^. currentSlot < toSlotNo (s ^. contractState . refundSlot - 1))
    Refund w -> s ^. currentSlot >= toSlotNo (s ^. contractState . refundSlot)
             && Nothing /= (s ^. contractState . contributions . at w)
    Pay _ v -> s ^. currentSlot < toSlotNo (s ^. contractState . refundSlot - 1)
            && Ada.adaValueOf (fromInteger v) `geq` Ada.toValue L.minAdaTxOutEstimated
    -- BadRefund w w' -> s ^. currentSlot < s ^. contractState . refundSlot - 2  -- why -2?
       --             || w /= w'

  arbitraryAction s = frequency $ [ (prefer beforeRefund,  Pay <$> genWallet <*> choose @Integer (10, 30))
                                  , (prefer beforeRefund,  Redeem <$> genWallet) ] ++
          --                         , (prefer afterRefund,   BadRefund <$> QC.elements testWallets <*> QC.elements testWallets) ] ++
                                  [ (prefer afterRefund,   Refund <$> QC.elements (s ^. contractState . contributions . to Map.keys))
                                  | Prelude.not . null $ s ^. contractState . contributions . to Map.keys ]
                  where
                    slot = s ^. currentSlot
                    beforeRefund = slot < toSlotNo (s ^. contractState . refundSlot)
                    afterRefund = Prelude.not beforeRefund
                    prefer b = if b then 10 else 1
                    genWallet   = QC.choose (1, length knownWallets)


-- | Tell us how to run an `AuctionModel` in the `SuperMockChain` - an
-- extension of the Cooked Validator `MockChain` monad adapted to
-- work with `QuickCheck.ContractModel`.
instance RunModel EscrowModel (SuperMockChain ()) where
  -- `perform` runs API actions by calling the off-chain code of
  -- the contract in the `SuperMockChain` monad.
  perform _ (Pay w v) _ = void $ do
    pay (typedValidator modelParams) (wallet w) modelParams (Ada.adaValueOf $ fromInteger v)
  perform _ (Redeem w) _ = void $ do
    redeem (typedValidator modelParams) (wallet w) modelParams
  perform _ (Refund w) _ = void $ do
    refund (typedValidator modelParams) (wallet w) modelParams


  -- `monitoring` gives us a way to apply `QuickCheck` monitoring
  -- functions like `classify` and `tabulate` to our property to
  -- get a better idea of the test case distribution. In this case
  -- we just track how many tests actually contain a `Hammer` action
  -- indicating that an auction has been finished.
{-  monitoring _ (Redeem _) = classify True "Contains Redeem"
  monitoring (_,_) (BadRefund w w') = tabulate "Bad refund attempts" [if w==w' then "early refund" else "steal refund"]
  monitoring (s,s') _ = classify (redeemable s' && Prelude.not (redeemable s)) "Redeemable"
    where redeemable s = precondition s (Redeem undefined) -}

-- | A standard property that tests that the balance changes
-- predicted by the `ContractModel` instance match the balance changes produced
-- by the `RunModel` instance - up to minimum ada requirements on UTxOs.
prop_Escrow :: Actions EscrowModel -> Property
prop_Escrow = propRunActions testInit () balanceChangePredicate

escrowParams :: L.POSIXTime -> EscrowParams d
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
                $ testSucceedsFrom def testInit redeemCheck,
        testProperty "prop_Escrow" prop_Escrow]

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
    void $ Cooked.awaitSlot $ deadlineSlot + 1
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


escrowParams' :: L.POSIXTime -> EscrowParams d
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
    void $ Cooked.awaitSlot $ deadlineSlot + 1
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
    void $ Cooked.awaitSlot $ deadlineSlot + 1
    payWallet (wallet 1) (wallet 2) (Ada.adaValueOf 920)
