{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Contract.ToUPLC where

import Data.ByteString (ByteString)
import Flat (flat)
import qualified Ledger.Typed.Scripts as Scripts
import Contract.Escrow
import qualified PlutusTx
import Ledger (POSIXTime, PaymentPubKeyHash (unPaymentPubKeyHash), TxId, getCardanoTxId)
import Plutus.V2.Ledger.Scripts qualified as Plutus


-- | Compiles the pmultisig contract down to a bytestring, that can later be loaded
--  as a arbitrary UPLC contract.

{-
escrowBS :: EscrowParams Datum -> ByteString
escrowBS escrow =
  flat $
    PlutusTx.getPlc $
      $$(PlutusTx.compile [||wrap||])
        `PlutusTx.applyCode` ( $$(PlutusTx.compile [||validate||])
                                 `PlutusTx.applyCode` PlutusTx.liftCode escrow @Escrow
                             )
  where
    wrap = Scripts.mkUntypedValidator
-}

escrowValidator :: Plutus.Validator
escrowValidator = validatorScript typedValidator


{-
      $$(PlutusTx.compile [||wrap||])
        `PlutusTx.applyCode` ( $$(PlutusTx.compile [||validate||])
                                 `PlutusTx.applyCode` PlutusTx.liftCode parms
                             )
  where
    wrap = Scripts.mkUntypedValidator @PaymentPubKeyHash @Action
-}

{-
typedValidator :: EscrowParams Datum -> V2.TypedValidator Escrow
typedValidator = go
  where
    go =
      V2.mkTypedValidatorParam @Escrow
        $$(PlutusTx.compile [||validate||])
        $$(PlutusTx.compile [||wrap||])
    wrap = Scripts.mkUntypedValidator


validatPayment :: PMultiSig -> Datum -> Redeemer -> Bool

{-# INLINEABLE pmultisig #-}
pmultisig :: Params -> Scripts.TypedValidator PMultiSig
pmultisig =
  Scripts.mkTypedValidatorParam @PMultiSig
    $$(PlutusTx.compile [||validatePayment||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.mkUntypedValidator @Datum @Redeemer

- | Compiles the pmultisig contract down to a bytestring, that can later be loaded
--  as a arbitrary UPLC contract.
pmultisigBS :: Params -> ByteString
pmultisigBS parms =
  flat $
    PlutusTx.getPlc $
      $$(PlutusTx.compile [||wrap||])
        `PlutusTx.applyCode` ( $$(PlutusTx.compile [||validatePayment||])
                                 `PlutusTx.applyCode` PlutusTx.liftCode parms
                             )
  where
    wrap = Scripts.mkUntypedValidator @Datum @Redeemer
-}

--  https://github.com/james-iohk/plutus-scripts/tree/master
