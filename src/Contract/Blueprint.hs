{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Contract.Blueprint where

import Data.ByteString (ByteString)
import Flat (flat)
import qualified Ledger.Typed.Scripts as Scripts
import Contract.Escrow
import qualified PlutusTx
import Ledger (POSIXTime, PaymentPubKeyHash (unPaymentPubKeyHash), TxId, getCardanoTxId)

import PlutusTx.Blueprint

import PlutusTx.Prelude

import Data.Set qualified as Set
import GHC.Generics (Generic)
import PlutusTx.Blueprint.TH (makeIsDataSchemaIndexed)
import Prelude (FilePath, IO)

escrowBlueprint :: ContractBlueprint
escrowBlueprint =
  MkContractBlueprint
    { contractId = Just "Escrow contract"
    , contractPreamble = escrowPreamble -- defined below
    , contractValidators = Set.singleton escrowValidator -- defined below
    , contractDefinitions = deriveDefinitions @[EscrowParams Datum, PaymentPubKeyHash, Action]
    }

escrowPreamble :: Preamble
escrowPreamble =
  MkPreamble
    { preambleTitle = "Escrow Contract"
    , preambleDescription = Just "A simple escrow contract"
    , preambleVersion = "1.3.0"
    , preamblePlutusVersion = PlutusV2
    , preambleLicense = Just "MIT"
    }

escrowValidator =
  MkValidatorBlueprint
    { validatorTitle = "Escrow Validator"
    , validatorDescription = Just "An escrow validator"
    , validatorParameters =
        [ MkParameterBlueprint
            { parameterTitle = Just "My Validator Parameters"
            , parameterDescription = Just "Compile-time validator parameters"
            , parameterPurpose = Set.singleton Spend
            , parameterSchema = definitionRef @(EscrowParams Datum)
            }
        ]
    , validatorRedeemer =
        MkArgumentBlueprint
          { argumentTitle = Just "My Redeemer"
          , argumentDescription = Just "A redeemer that does something awesome"
          , argumentPurpose = Set.fromList [Spend, Mint]
          , argumentSchema = definitionRef @Action
          }
    , validatorDatum =
        Just
          MkArgumentBlueprint
            { argumentTitle = Just "My Datum"
            , argumentDescription = Just "A datum that contains something awesome"
            , argumentPurpose = Set.singleton Spend
            , argumentSchema = definitionRef @PaymentPubKeyHash
            }
    , validatorCompiledCode = Nothing -- you can optionally provide the compiled code here
    }
