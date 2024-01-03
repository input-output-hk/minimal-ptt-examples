{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -g -fplugin-opt PlutusTx.Plugin:coverage-all #-}
module Contract.Vesting (
    -- $vesting
    VestingParams(..),
    -- VestingSchema,
    VestingTranche(..),
    VestingError(..),
    AsVestingError(..),
    totalAmount,
    -- vestingContract,
    validate,
    vestingScript,
    covIdx) where

{-
import Control.Lens
import Control.Monad (void, when)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map qualified as Map
import Prelude (Semigroup (..))


import Ledger (CardanoAddress, POSIXTime, POSIXTimeRange, PaymentPubKeyHash (unPaymentPubKeyHash),
               decoratedTxOutPlutusValue)
-}

-- import Cardano.Node.Emulator.Params (pNetworkId, testnet)
-- import Ledger.Interval qualified as Interval
-- import Ledger.Tx.Constraints (TxConstraints, mustBeSignedBy, mustPayToTheScriptWithDatumInTx, mustValidateInTimeRange)
-- import Ledger.Tx.Constraints qualified as Constraints
-- import Ledger.Tx.Constraints.ValidityInterval qualified as ValidityInterval
-- import Plutus.Contract
-- import Plutus.Contract.Test.Coverage.Analysis
-- import Plutus.V2.Ledger.Api (ScriptContext (..), TxInfo (..), Validator)
-- import Plutus.V2.Ledger.Contexts qualified as V2
{-
import Ledger.Typed.Scripts (ValidatorTypes (..))
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Script.Utils.V2.Typed.Scripts qualified as V2
import Plutus.Script.Utils.Value (Value)
import Plutus.Script.Utils.Value qualified as Value
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup (..), fold)

-}

import Control.Lens (makeClassyPrisms)
import Control.Monad (void)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.RWS.Class (asks)
import Data.Map qualified as Map

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import PlutusTx (ToData)
import PlutusTx qualified
import PlutusTx.Code (getCovIdx)
import PlutusTx.Coverage (CoverageIndex)
import PlutusTx.Prelude (Bool)


import Cardano.Node.Emulator qualified as E
import Cardano.Node.Emulator.Internal.Node (
  SlotConfig,
  pSlotConfig,
  posixTimeRangeToContainedSlotRange,
 )
import Cardano.Node.Emulator.Test (testnet)
import Data.Maybe (fromJust)
import Ledger (POSIXTime, PaymentPubKeyHash (unPaymentPubKeyHash), TxId, getCardanoTxId)
import Ledger qualified
import Ledger.Tx.CardanoAPI qualified as C
import Ledger.Typed.Scripts (validatorCardanoAddress)
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Script.Utils.Scripts (ValidatorHash, datumHash)
import Plutus.Script.Utils.V2.Contexts (
  ScriptContext (ScriptContext, scriptContextTxInfo),
  TxInfo,
  scriptOutputsAt,
  txInfoValidRange,
  txSignedBy,
 )
import Plutus.Script.Utils.V2.Contexts qualified as V2
import Plutus.Script.Utils.V2.Typed.Scripts qualified as V2
import Plutus.Script.Utils.Value (Value, geq, lt)
import PlutusLedgerApi.V1.Interval qualified as Interval
import PlutusLedgerApi.V2 (Datum (Datum))
import PlutusLedgerApi.V2.Contexts (valuePaidTo)
import PlutusLedgerApi.V2.Contexts qualified as V2
import PlutusLedgerApi.V2.Tx (OutputDatum (OutputDatum))

-- My imports

import GHC.Generics (Generic)
import Prelude qualified as Haskell
import PlutusTx.Prelude -- qualified as PlutusTx

{- |
    A simple vesting scheme. Money is locked by a contract and may only be
    retrieved after some time has passed.

    This is our first example of a contract that covers multiple transactions,
    with a contract state that changes over time.

    In our vesting scheme the money will be released in two _tranches_ (parts):
    A smaller part will be available after an initial number of time has
    passed, and the entire amount will be released at the end. The owner of the
    vesting scheme does not have to take out all the money at once: They can
    take out any amount up to the total that has been released so far. The
    remaining funds stay locked and can be retrieved later.

    Let's start with the data types.

-}

data Vesting

instance Scripts.ValidatorTypes Vesting where
    type instance RedeemerType Vesting = ()
    type instance DatumType Vesting = ()

-- | Tranche of a vesting scheme.
data VestingTranche = VestingTranche {
    vestingTrancheDate   :: POSIXTime,
    vestingTrancheAmount :: Value
    }
    -- deriving Generic

PlutusTx.makeLift ''VestingTranche

-- | A vesting scheme consisting of two tranches. Each tranche defines a date
--   (POSIX time) after which an additional amount can be spent.
data VestingParams = VestingParams {
    vestingTranche1 :: VestingTranche,
    vestingTranche2 :: VestingTranche,
    vestingOwner    :: PaymentPubKeyHash
    } deriving Generic

PlutusTx.makeLift ''VestingParams

{-# INLINABLE valueLockedBy #-}
-- | Get the total value locked by the given validator in this transaction.
valueLockedBy :: TxInfo -> ValidatorHash -> Value
valueLockedBy ptx h =
    let outputs = map snd (scriptOutputsAt h ptx)
    in mconcat outputs

{-# INLINABLE totalAmount #-}
-- | The total amount vested
totalAmount :: VestingParams -> Value
totalAmount VestingParams{vestingTranche1,vestingTranche2} =
    vestingTrancheAmount vestingTranche1 + vestingTrancheAmount vestingTranche2

{-# INLINABLE availableFrom #-}
-- | The amount guaranteed to be available from a given tranche in a given time range.
availableFrom :: VestingTranche -> Ledger.POSIXTimeRange -> Value
availableFrom (VestingTranche d v) range =
    -- The valid range is an open-ended range starting from the tranche vesting date
    let validRange = Interval.from d
    -- If the valid range completely contains the argument range (meaning in particular
    -- that the start time of the argument range is after the tranche vesting date), then
    -- the money in the tranche is available, otherwise nothing is available.
    in if validRange `Interval.contains` range then v else zero

availableAt :: VestingParams -> POSIXTime -> Value
availableAt VestingParams{vestingTranche1, vestingTranche2} time =
    let f VestingTranche{vestingTrancheDate, vestingTrancheAmount} =
            if time >= vestingTrancheDate then vestingTrancheAmount else mempty
    in foldMap f [vestingTranche1, vestingTranche2]

{-# INLINABLE remainingFrom #-}
-- | The amount that has not been released from this tranche yet
remainingFrom :: VestingTranche -> Ledger.POSIXTimeRange -> Value
remainingFrom t@VestingTranche{vestingTrancheAmount} range =
    vestingTrancheAmount - availableFrom t range

{-# INLINABLE validate #-}
validate :: VestingParams -> () -> () -> ScriptContext -> Bool
validate VestingParams{vestingTranche1, vestingTranche2, vestingOwner} () () ctx@ScriptContext{scriptContextTxInfo=txInfo@V2.TxInfo{txInfoValidRange}} =
    let
        remainingActual  = valueLockedBy txInfo (V2.ownHash ctx)

        remainingExpected =
            remainingFrom vestingTranche1 txInfoValidRange
            + remainingFrom vestingTranche2 txInfoValidRange

    in remainingActual `geq` remainingExpected
            -- The policy encoded in this contract
            -- is "vestingOwner can do with the funds what they want" (as opposed
            -- to "the funds must be paid to vestingOwner"). This is enforcey by
            -- the following condition:
            && txSignedBy txInfo (unPaymentPubKeyHash vestingOwner)
            -- That way the recipient of the funds can pay them to whatever address they
            -- please, potentially saving one transaction.

vestingScript :: VestingParams -> V2.Validator
vestingScript = Scripts.validatorScript . typedValidator

typedValidator :: VestingParams -> V2.TypedValidator Vesting
typedValidator = V2.mkTypedValidatorParam @Vesting
    $$(PlutusTx.compile [|| validate ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator

contractAddress :: VestingParams -> Ledger.CardanoAddress
contractAddress = Scripts.validatorCardanoAddress testnet . typedValidator

data VestingError =
    VContractError
    | InsufficientFundsError Value Value Value
    deriving stock (Haskell.Show)
    -- deriving stock (Haskell.Eq, Haskell.Show, Generic)
    -- deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''VestingError

{-
-- instance AsContractError VestingError where
--    _ContractError = _VContractError
-- | Pay some money into the vesting contract.
mkPayTx
  :: SlotConfig
  -> VestingParams
  -- ^ The escrow contract
  -> Ledger.CardanoAddress
  -- ^ Wallet address
  -> Value
  -- ^ How much money to pay in
  -> (C.CardanoBuildTx, Ledger.UtxoIndex)
mkPayTx slotConfig vesting wallet vl =
  let vestingAddr = contractAddress vesting
      pkh = Ledger.PaymentPubKeyHash $ fromJust $ Ledger.cardanoPubKeyHash wallet
      txOut = C.TxOut vestingwAddr (toTxOutValue vl) (toTxOutInlineDatum pkh) C.ReferenceScriptNone
      validityRange = toValidityRange slotConfig $ Interval.to $ escrowDeadline escrow - 1000
      utx =
        E.emptyTxBodyContent
          { C.txOuts = [txOut]
          , C.txValidityLowerBound = fst validityRange
          , C.txValidityUpperBound = snd validityRange
          }
      utxoIndex = mempty
   in (C.CardanoBuildTx utx, utxoIndex)

vestFundsC
    :: (E.MonadEmulator m)
    => Ledger.CardanoAddress
    -> Ledger.PaymentPrivateKey
    -> VestingParams
    -> m ()
vestFundsC wallet privateKey vesting = do
  E.logInfo @String $ "Vest"




  mapError (review _VestingError) $ do
    let tx = payIntoContract (totalAmount vesting)
    mkTxConstraints (Constraints.typedValidatorLookups $ typedValidator vesting) tx
      >>= adjustUnbalancedTx >>= void . submitUnbalancedTx
-}


{-
payIntoContract :: Value -> TxConstraints () ()
payIntoContract = mustPayToTheScriptWithDatumInTx ()

vestFundsC
    :: ( AsVestingError e
       )
    => VestingParams
    -> Contract w s e ()
vestFundsC vesting = mapError (review _VestingError) $ do
    let tx = payIntoContract (totalAmount vesting)
    mkTxConstraints (Constraints.typedValidatorLookups $ typedValidator vesting) tx
      >>= adjustUnbalancedTx >>= void . submitUnbalancedTx

data Liveness = Alive | Dead

retrieveFundsC
    :: ( AsVestingError e
       )
    => VestingParams
    -> Value
    -> Contract w s e Liveness
retrieveFundsC vesting payment = mapError (review _VestingError) $ do
    networkId <- pNetworkId <$> getParams
    let inst = typedValidator vesting
        addr = Scripts.validatorCardanoAddress networkId inst
    now <- fst <$> currentNodeClientTimeRange
    unspentOutputs <- utxosAt addr
    let
        currentlyLocked = foldMap decoratedTxOutPlutusValue (Map.elems unspentOutputs)
        remainingValue = currentlyLocked - payment
        mustRemainLocked = totalAmount vesting - availableAt vesting now
        maxPayment = currentlyLocked - mustRemainLocked

    when (remainingValue `Value.lt` mustRemainLocked)
        $ throwError
        $ InsufficientFundsError payment maxPayment mustRemainLocked

    let liveness = if remainingValue `Value.gt` mempty then Alive else Dead
        remainingOutputs = case liveness of
                            Alive -> payIntoContract remainingValue
                            Dead  -> mempty
        tx = Constraints.spendUtxosFromTheScript unspentOutputs ()
                <> remainingOutputs
                <> mustValidateInTimeRange (ValidityInterval.from now)
                <> mustBeSignedBy (vestingOwner vesting)
                -- we don't need to add a pubkey output for 'vestingOwner' here
                -- because this will be done by the wallet when it balances the
                -- transaction.
    mkTxConstraints (Constraints.typedValidatorLookups inst
                  <> Constraints.unspentOutputs unspentOutputs) tx
      >>= adjustUnbalancedTx >>= void . submitUnbalancedTx
    return liveness
-}

covIdx :: CoverageIndex
covIdx = getCovIdx $$(PlutusTx.compile [||validate||])

-- covIdx :: CoverageIndex
-- covIdx = computeRefinedCoverageIndex $$(PlutusTx.compile [|| \a b c d -> check (validate a b c d) ||])
