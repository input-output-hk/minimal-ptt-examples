module Test.QuickCheck.ContractModel.PlutusCompat where

import Control.Arrow (first)

import Data.Fixed (Fixed (MkFixed), Micro)

import System.Random
import System.Random.Internal

import Cardano.Api
import Cardano.Crypto.Hash.Class    qualified as Crypto

import Plutus.Script.Utils.Ada qualified as Plutus
import Plutus.V1.Ledger.Value  qualified as Plutus hiding (adaSymbol, adaToken)
import PlutusTx.AssocMap       qualified as AssocMap
import PlutusTx.Builtins       qualified as Builtins

import Ledger.CardanoWallet qualified as Plutus
import Ledger.Index         qualified as Plutus
import Ledger.Slot          qualified as Plutus
import Ledger.Tx            qualified as Plutus
import Ledger.Tx.CardanoAPI qualified as Plutus

import Test.QuickCheck.ContractModel
import Test.QuickCheck.ContractModel.Internal.Model
import Test.QuickCheck.StateModel

-- TODO:
-- * make plutus-apps depend on this to avoid needlessly re-written code

deriving via BaseType Plutus.MockWallet instance HasSymbolics Plutus.MockWallet
deriving via BaseType SlotNo instance HasSymbolics SlotNo
deriving via BaseType Micro instance HasSymbolics Micro

deriving via HasNoVariables Micro instance HasVariables Micro

instance Uniform SlotNo where
  uniformM g = SlotNo <$> uniformM g

instance UniformRange SlotNo where
  uniformRM (SlotNo r0, SlotNo r1) g = SlotNo <$> uniformRM (r0, r1) g

instance Random SlotNo where

instance UniformRange Micro where
  uniformRM (MkFixed r0, MkFixed r1) g = MkFixed <$> uniformRM (r0, r1) g

instance Random Micro where
  random = first MkFixed . random

fromValue :: Plutus.Value -> Value
fromValue (Plutus.Value m) = valueFromList [ (toAssetId (Plutus.AssetClass (cls, sym)), fromIntegral n)
                                           | (cls, toks) <- AssocMap.toList m
                                           , (sym, n)    <- AssocMap.toList toks
                                           ]

instance SymValueLike Plutus.Value where
  toSymValue = toSymValue . fromValue

instance TokenLike Plutus.AssetClass where
  symAssetIdValueOf v = symAssetIdValueOf v . toAssetId
  symAssetIdValue = symAssetIdValue . toAssetId

instance SymValueLike Plutus.Ada where
  toSymValue = toSymValue . Plutus.toValue

fromCardanoTx :: Plutus.CardanoTx -> Tx Era
fromCardanoTx (Plutus.CardanoEmulatorEraTx tx) = tx

fromUtxoIndex :: Plutus.UtxoIndex -> UTxO Era
fromUtxoIndex = id

mkTxOut :: Plutus.TxOut -> TxOut CtxUTxO Era
mkTxOut (Plutus.TxOut o) = toCtxUTxOTxOut o

makeTheHash :: Crypto.HashAlgorithm crypto => Builtins.BuiltinByteString -> Crypto.Hash crypto stuff
makeTheHash bs =
  case Crypto.hashFromBytes $ Builtins.fromBuiltin bs of
    Nothing   -> error "Bad hash!"
    Just hash -> hash

toAssetId :: Plutus.AssetClass -> AssetId
toAssetId (Plutus.AssetClass (sym, tok))
  | sym == Plutus.adaSymbol, tok == Plutus.adaToken = AdaAssetId
  | otherwise                                       = AssetId (toPolicyId sym) (toAssetName tok)

toPolicyId :: Plutus.CurrencySymbol -> PolicyId
toPolicyId sym@(Plutus.CurrencySymbol bs)
  | Just hash <- deserialiseFromRawBytes AsScriptHash (Builtins.fromBuiltin bs) = PolicyId hash
  | otherwise = error $ "Bad policy id: " ++ show sym

toAssetName :: Plutus.TokenName -> AssetName
toAssetName (Plutus.TokenName bs) = AssetName $ Builtins.fromBuiltin bs

fromSlotNo :: SlotNo -> Plutus.Slot
fromSlotNo (SlotNo n) = Plutus.Slot $ fromIntegral n

toSlotNo :: Plutus.Slot -> SlotNo
toSlotNo (Plutus.Slot n) = SlotNo $ fromIntegral n

toTxIn :: Plutus.TxOutRef -> TxIn
toTxIn = either (error . show) id . Plutus.toCardanoTxIn
