{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Spec.Vesting (VestingModel
                    , prop_Vesting
                    , prop_Check) where

            {-        , tests
                    , modelTests
                    , prop_Vesting
                    , prop_CheckNoLockedFundsProof
                    , retrieveFundsTrace
                    , certification
                    , check_propVestingWithCoverage) where -}

----Additional

import PlutusTx.Numeric qualified as Numeric
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.Script.Utils.Value

-----

import Control.Lens (At (at), makeLenses, to, (%=), (.=), (^.))
import Control.Monad (void, when)
import Control.Monad.Trans (lift)
import Data.Default (Default (def))
import Data.Foldable (Foldable (fold, length, null), sequence_)
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.Generics (Generic)

import Cardano.Api.Shelley (toPlutusData)
import Cardano.Node.Emulator qualified as E
import Cardano.Node.Emulator.Internal.Node.Params qualified as Params
import Cardano.Node.Emulator.Internal.Node.TimeSlot qualified as TimeSlot
import Cardano.Node.Emulator.Test (
  checkPredicateOptions,
  hasValidatedTransactionCountOfTotal,
  walletFundsChange,
  (.&&.),
 )
import Cardano.Node.Emulator.Test.Coverage (writeCoverageReport)
import Cardano.Node.Emulator.Test.NoLockedFunds (
  NoLockedFundsProof (nlfpMainStrategy, nlfpWalletStrategy),
  checkNoLockedFundsProofWithOptions,
  defaultNLFP,
 )
import Ledger (Slot, minAdaTxOutEstimated)
import Ledger qualified
import Ledger.Tx.CardanoAPI (fromCardanoSlotNo)
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value.CardanoAPI qualified as Value
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.Script.Utils.Value (Value, geq)
import PlutusLedgerApi.V1.Time (POSIXTime)

import Contract.Vesting (
  VestingParams(..),
  vestFunds,
  retrieveFunds,
  typedValidator,
  VestingTranche(..),
 )
import PlutusTx (fromData)
import PlutusTx.Monoid (inv)

import Cardano.Api (
  AddressInEra (AddressInEra),
  AllegraEraOnwards (AllegraEraOnwardsBabbage),
  IsShelleyBasedEra (shelleyBasedEra),
  TxOut (TxOut),
  TxValidityLowerBound (TxValidityLowerBound, TxValidityNoLowerBound),
  TxValidityUpperBound (TxValidityUpperBound),
  UTxO (unUTxO),
  toAddressAny,
 )
import Test.QuickCheck qualified as QC hiding ((.&&.))
import Test.QuickCheck.ContractModel (
  Action,
  Actions,
  ContractModel,
  DL,
  RunModel,
  action,
  anyActions_,
  assertModel,
  contractState,
  currentSlot,
  deposit,
  forAllDL,
  lockedValue,
  observe,
  symIsZero,
  utxo,
  viewContractState,
  viewModelState,
  wait,
  waitUntilDL,
  withdraw,
 )
import Test.QuickCheck.ContractModel qualified as QCCM
import Test.QuickCheck.ContractModel.ThreatModel (
  IsInputOrOutput (addressOf),
  ThreatModel,
  anyInputSuchThat,
  changeValidityRange,
  getRedeemer,
  shouldNotValidate,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (
  Property,
  choose,
  frequency,
  testProperty,
 )

import Plutus.Script.Utils.Value (leq)

w1, w2, w3, w4, w5 :: Wallet
w1 = 1
w2 = 2
w3 = 3
w4 = 4
w5 = 5

walletAddress :: Wallet -> Ledger.CardanoAddress
walletAddress = (E.knownAddresses !!) . pred . fromIntegral

walletPrivateKey :: Wallet -> Ledger.PaymentPrivateKey
walletPrivateKey = (E.knownPaymentPrivateKeys !!) . pred . fromIntegral

testWallets :: [Wallet]
testWallets = [w1, w2, w3, w4, w5] -- removed five to increase collisions (, w6, w7, w8, w9, w10])

walletPaymentPubKeyHash :: Wallet -> Ledger.PaymentPubKeyHash
walletPaymentPubKeyHash =
  Ledger.PaymentPubKeyHash
    . Ledger.pubKeyHash
    . Ledger.unPaymentPubKey
    . (E.knownPaymentPublicKeys !!)
    . pred
    . fromIntegral

-- | The scenario used in the property tests. It sets up a vesting scheme for a
--   total of 60 ada over 20 blocks (20 ada can be taken out before
--   that, at 10 blocks).
vesting :: POSIXTime -> VestingParams
vesting startTime =
    VestingParams
        { vestingTranche1 = VestingTranche (startTime + 10000) (Ada.adaValueOf 20)
        , vestingTranche2 = VestingTranche (startTime + 20000) (Ada.adaValueOf 40)
        , vestingOwner    = walletPaymentPubKeyHash w1 }

params :: VestingParams
params = vesting (TimeSlot.scSlotZeroTime def)

-- * QuickCheck model

type Wallet = Integer

data VestingModel =
  VestingModel { _vestedAmount :: Value -- ^ How much value is in the contract
               , _vested       :: [Wallet] -- ^ What wallets have already vested money
               , _t1Slot       :: Slot -- ^ The time for the first tranche
               , _t2Slot       :: Slot -- ^ The time for the second tranche
               , _t1Amount     :: Value -- ^ The size of the first tranche
               , _t2Amount     :: Value -- ^ The size of the second tranche
               } deriving (Show, Eq, Generic)

makeLenses 'VestingModel

-- deriving instance Eq (ContractInstanceKey VestingModel w schema err params)
-- deriving instance Show (ContractInstanceKey VestingModel w schema err params)

-- This instance models the behaviour of the vesting contract. There are some peculiarities
-- that stem from the implementation of the contract that are apparent in the precondition
-- to the `Vest` endpoint.
instance ContractModel VestingModel where
  -- data ContractInstanceKey VestingModel w schema err params where
    -- WalletKey :: Wallet -> ContractInstanceKey VestingModel () VestingSchema VestingError ()

  data Action VestingModel = Vest Wallet
                           | Retrieve Wallet Value
                           deriving (Eq, Show, Generic)

  initialState = VestingModel
    { _vestedAmount = mempty
    , _vested       = []
    , _t1Slot       = TimeSlot.posixTimeToEnclosingSlot def $ vestingTrancheDate (vestingTranche1 params)
    , _t2Slot       = TimeSlot.posixTimeToEnclosingSlot def $ vestingTrancheDate (vestingTranche2 params)
    , _t1Amount     = vestingTrancheAmount (vestingTranche1 params)
    , _t2Amount     = vestingTrancheAmount (vestingTranche2 params) }

  -- initialInstances = (`StartContract` ()) . WalletKey <$> [w1, w2, w3]

  -- instanceWallet (WalletKey w) = w

  -- instanceContract _ WalletKey{} _ = vestingContract params

  -- Vest the sum of the two tranches
  nextState (Vest w) = do
    let amount =  vestingTrancheAmount (vestingTranche1 params)
               <> vestingTrancheAmount (vestingTranche2 params)
    withdraw (walletAddress w) amount
    vestedAmount %= (<> amount)
    vested       %= (w:)
    wait 1

  -- Retrieve `v` value as long as that leaves enough value to satisfy
  -- the tranche requirements
  nextState (Retrieve w v) = do
    slot   <- viewModelState currentSlot
    amount <- viewContractState vestedAmount
    let newAmount = amount Numeric.- v
    s      <- viewContractState id
    when ( enoughValueLeft (fromCardanoSlotNo slot) s v
         && v `leq` amount
         && walletPaymentPubKeyHash w == vestingOwner params
         && Ada.fromValue v >= Ledger.minAdaTxOutEstimated
         && (Ada.fromValue newAmount == 0 || Ada.fromValue newAmount >= Ledger.minAdaTxOutEstimated)) $ do
      deposit (walletAddress w) v
      vestedAmount .= newAmount
    wait 2

  precondition s (Vest w) =  w `notElem` s ^. contractState . vested -- After a wallet has vested the contract shuts down
                          && walletPaymentPubKeyHash w /= vestingOwner params -- The vesting owner shouldn't vest
                          && (fromCardanoSlotNo slot) < t1 -- If you vest after slot 1 it can cause the vesting owner to terminate prematurely
    where
      slot   = s ^. currentSlot
      t1     = s ^. contractState . t1Slot

  precondition s (Retrieve w v) = enoughValueLeft (fromCardanoSlotNo slot) (s ^. contractState) v
                                && walletPaymentPubKeyHash w == vestingOwner params
                                && Ada.fromValue v >= Ledger.minAdaTxOutEstimated
                                && (Ada.fromValue newAmount == 0 || Ada.fromValue newAmount >= Ledger.minAdaTxOutEstimated)
    where
      slot   = s ^. currentSlot
      amount = s ^. contractState . vestedAmount
      newAmount = amount Numeric.- v

  arbitraryAction s = frequency [ (1, Vest <$> genWallet)
                                , (1, Retrieve <$> genWallet
                                               <*> (Ada.lovelaceValueOf
                                                   <$> choose (Ada.getLovelace Ledger.minAdaTxOutEstimated, valueOf amount Ada.adaSymbol Ada.adaToken)
                                                   )
                                  )
                                ]
    where
      amount   = s ^. contractState . vestedAmount


instance RunModel VestingModel E.EmulatorM where
  perform _ cmd _ = lift $ act cmd

act :: Action VestingModel -> E.EmulatorM ()
act = \case
  Vest w -> do
    vestFunds
      (walletAddress w)
      (walletPrivateKey w)
      params
  Retrieve w v -> do
    void $
      retrieveFunds
        (walletAddress w)
        (walletPrivateKey w)
        params
        v -- (Ada.adaValueOf $ fromInteger v)

-- Fix This:
--  shrinkAction _ (Vest _)       = []
--  shrinkAction _ (Retrieve w v) = Retrieve w <$> shrinkValue v

-- | Check that the amount of value left in the contract
-- is at least the amount that remains locked at the current
-- slot.
enoughValueLeft :: Slot -- ^ current slot
                -> VestingModel
                -> Value
                -> Bool
enoughValueLeft slot s take =
  let availableValue   = mconcat $ [ t1v | slot > t1 ] ++ [ t2v | slot > t2 ]
      totalValue       = t1v <> t2v
      mustRemainLocked = totalValue Numeric.- availableValue
  in mustRemainLocked `leq` (vested Numeric.- take)
  where
    vested = s ^. vestedAmount
    t1     = s ^. t1Slot
    t1v    = s ^. t1Amount
    t2     = s ^. t2Slot
    t2v    = s ^. t2Amount

genWallet :: QC.Gen Wallet
genWallet = QC.elements testWallets

prop_Vesting :: Actions VestingModel -> Property
prop_Vesting = E.propRunActions

simpleVestTest :: DL VestingModel ()
simpleVestTest = do
              action $ Vest w2
              waitUntilDL 11
              action $ Retrieve w1 (Ada.adaValueOf 10)


simpleVestTest' :: DL VestingModel ()
simpleVestTest' = do
        action $ Vest w3
        action $ Vest w4
        waitUntilDL 9
        action $ Retrieve w1 (Ada.adaValueOf 10)
        action $ Retrieve w1 (Ada.adaValueOf 10)


prop_Check :: QC.Property
prop_Check = QC.withMaxSuccess 1 $ forAllDL simpleVestTest' prop_Vesting

{-
Balance changes don't match:
  Predicted symbolic balance changes:
    Wallet 2: {60000000 Lovelace}
    Wallet 1: {10000000 Lovelace}
  Predicted actual balance changes:
    Wallet 2: {60000000 Lovelace}
    Wallet 1: {10000000 Lovelace}
  Actual balance changes:
    Wallet 2: {60000000 Lovelace}
    Wallet 1: {50000000 Lovelace}
-}

{-
simpleVestTest :: DL VestingModel ()
simpleVestTest = do
 [[+]Vest 2,
  [+]Vest 4,
  [+]Vest 3,
  [+]Retrieve 1 (Value {getValue = Map {unMap = [(,Map {unMap = [("",67263905)]})]}})]
-}


{-
shrinkValue :: Value -> [Value]
shrinkValue v = Ada.lovelaceValueOf <$> filter (\val -> val >= Ada.getLovelace Ledger.minAdaTxOutEstimated) (shrink (valueOf v Ada.adaSymbol Ada.adaToken))

prop_Vesting :: Actions VestingModel -> Property
prop_Vesting = propRunActions_

noLockProof :: NoLockedFundsProof VestingModel
noLockProof = defaultNLFP {
      nlfpMainStrategy   = mainStrat,
      nlfpWalletStrategy = walletStrat }
    where
        -- To get all the money out simply wait until after the
        -- deadline and take as much money as has been vested.
        mainStrat = do
            amount <- viewContractState vestedAmount
            t2     <- viewContractState t2Slot
            slot   <- viewModelState currentSlot
            when (slot < t2 + Slot 1) $ do
              waitUntilDL $ t2 + Slot 1
            when (amount `gt` mempty) $ do
              action (Retrieve w1 amount)

        -- No one but w1 ever gets any money out of the contract.
        walletStrat w | w == w1 = mainStrat
                      | otherwise = return ()

prop_CheckNoLockedFundsProof :: Property
prop_CheckNoLockedFundsProof = checkNoLockedFundsProof noLockProof

-- Tests

tests :: TestTree
tests =
    let con = vestingContract (vesting startTime) in
    testGroup "vesting"
    [ checkPredicate "secure some funds with the vesting script"
        (walletFundsChangePlutus w2 (Numeric.negate $ totalAmount $ vesting startTime))
        $ do
            hdl <- Trace.activateContractWallet w2 con
            Trace.callEndpoint @"vest funds" hdl ()
            void $ Trace.waitNSlots 1

    , checkPredicate "retrieve some funds"
        (walletFundsChangePlutus w2 (Numeric.negate $ totalAmount $ vesting startTime)
        .&&. assertNoFailedTransactions
        .&&. walletFundsChangePlutus w1 (Ada.adaValueOf 10))
        retrieveFundsTrace

    , checkPredicate "cannot retrieve more than allowed"
        (walletFundsChangePlutus w1 mempty
        .&&. assertContractError con (Trace.walletInstanceTag w1) (== expectedError) "error should match")
        $ do
            hdl1 <- Trace.activateContractWallet w1 con
            hdl2 <- Trace.activateContractWallet w2 con
            Trace.callEndpoint @"vest funds" hdl2 ()
            Trace.waitNSlots 10
            Trace.callEndpoint @"retrieve funds" hdl1 (Ada.adaValueOf 30)
            void $ Trace.waitNSlots 1

    , checkPredicate "can retrieve everything at the end"
        (walletFundsChangePlutus w1 (totalAmount $ vesting startTime)
        .&&. assertNoFailedTransactions
        .&&. assertDone con (Trace.walletInstanceTag w1) (const True) "should be done")
        $ do
            hdl1 <- Trace.activateContractWallet w1 con
            hdl2 <- Trace.activateContractWallet w2 con
            Trace.callEndpoint @"vest funds" hdl2 ()
            Trace.waitNSlots 20
            Trace.callEndpoint @"retrieve funds" hdl1 (totalAmount $ vesting startTime)
            void $ Trace.waitNSlots 2

    , goldenPir "test/Spec/vesting.pir" $$(PlutusTx.compile [|| validate ||])
    , HUnit.testCaseSteps "script size is reasonable" $ \step -> reasonable' step (vestingScript $ vesting startTime) 33000
    ]

    where
        startTime = TimeSlot.scSlotZeroTime def

modelTests :: TestTree
modelTests =
    testGroup "vesting model tests"
    [
      testProperty "prop_Vesting" $ withMaxSuccess 20 prop_Vesting
      , testProperty "prop_CheckNoLockedFundsProof" $ withMaxSuccess 20 prop_CheckNoLockedFundsProof
    -- TODO: re-activate when double satisfaction is turned on again
    -- , testProperty "prop_doubleSatisfaction" $ withMaxSuccess 20 prop_doubleSatisfaction
    ]

-- TODO: re-activate when double satisfaction is turned on again
-- prop_doubleSatisfaction :: Actions VestingModel -> Property
-- prop_doubleSatisfaction = checkDoubleSatisfaction

retrieveFundsTrace :: EmulatorTrace ()
retrieveFundsTrace = do
    startTime <- TimeSlot.scSlotZeroTime <$> Trace.getSlotConfig
    let con = vestingContract (vesting startTime)
    hdl1 <- Trace.activateContractWallet w1 con
    hdl2 <- Trace.activateContractWallet w2 con
    Trace.callEndpoint @"vest funds" hdl2 ()
    Trace.waitNSlots 10
    Trace.callEndpoint @"retrieve funds" hdl1 (Ada.adaValueOf 10)
    void $ Trace.waitNSlots 2

expectedError :: VestingError
expectedError =
    let payment = Ada.adaValueOf 30
        maxPayment = Ada.adaValueOf 20
        mustRemainLocked = Ada.adaValueOf 40
    in InsufficientFundsError payment maxPayment mustRemainLocked

instance CrashTolerance VestingModel where
  available a          alive = (Key $ WalletKey w) `elem` alive
    where w = case a of
                Vest w        -> w
                Retrieve w _  -> w

  restartArguments _ WalletKey{} = ()


prop_CrashTolerance :: Actions (WithCrashTolerance VestingModel) -> Property
prop_CrashTolerance = propRunActions_

check_propVestingWithCoverage :: IO ()
check_propVestingWithCoverage = do
  cr <- quickCheckWithCoverage stdArgs (set coverageIndex covIdx defaultCoverageOptions) $ \covopts ->
    withMaxSuccess 50 $ propRunActionsWithOptions @VestingModel defaultCheckOptions covopts (const (pure True))
  writeCoverageReport "Vesting" cr


simpleVestTest :: DL VestingModel ()
simpleVestTest = do
              action $ Vest w2
              waitUntilDL 11
              action $ Retrieve w1 (Ada.adaValueOf 10)
-}
