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
module Spec.Lotto where

import Control.Lens
import Control.Monad.Error

import Data.Default
import GHC.Generics hiding (to)

import Plutus.Script.Utils.Ada qualified as Ada
import Test.QuickCheck qualified as QC
import Test.QuickCheck.ContractModel hiding (inv)
import Test.QuickCheck.ContractModel.Cooked
import Test.QuickCheck.ContractModel.ThreatModel
import Test.QuickCheck.ThreatModel.DoubleSatisfaction
import Test.Tasty
import Test.Tasty.QuickCheck hiding (scale)

import Cooked.Wallet

import qualified Ledger as L
import Data.Map (Map)
import qualified Data.Map as Map
import Cardano.Node.Emulator.TimeSlot qualified as TimeSlot
import Cooked hiding (currentSlot)
import Test.Tasty.HUnit
import Plutus.V1.Ledger.Value hiding (adaSymbol, adaToken)

import Plutus.V1.Ledger.Value qualified as Plutus hiding (adaSymbol, adaToken)

-- Needed for threat model
import Ledger.Typed.Scripts qualified as Scripts
import Cardano.Node.Emulator.Params qualified as Params
import Cardano.Api.Shelley (toPlutusData)
import PlutusTx (fromData)
import Cardano.Api hiding (Value)
import Test.QuickCheck.ContractModel.ThreatModel qualified as TM

import PlutusTx.Prelude (BuiltinByteString, unsafeRatio, (-))
import Data.ByteString qualified as BS
import PlutusTx.Builtins       qualified as Builtins


import Data.Text qualified as T
import Data.Text.Encoding qualified as T

import qualified Lib
import qualified Lotto


import Cooked.Wallet

import Prelude hiding ((-))
import qualified Plutus.V2.Ledger.Api as LedgerV2

import Ledger.Tx.CardanoAPI qualified as Plutus
import Data.Maybe

import qualified Plutus.Script.Utils.Typed as TScripts

import qualified PlutusTx.AssocMap as AssocMap




-- | initial distribution
testInit :: InitialDistribution
testInit = initialDistribution [(i, [Ada.lovelaceValueOf 20_000_000]) | i <- knownWallets]

data LottoModel = LottoModel { _guesses       :: [(Int, String)]
                             -- , _refundSlot    :: L.Slot
                             , _secret        :: String
                             , _salt          :: String
                             , _txIn          :: Maybe SymTxIn
                             , _token         :: Maybe SymToken -- look into burning
                             , _value         :: Integer
--                             , _value         :: Maybe SymValue
                             -- , _tName         :: String
                             , _endSlot       :: SlotNo
                             , _phase         :: Phase
                             } deriving (Eq, Show, Generic)

data Phase = Initial | Minting | Playing | Resolving deriving (Eq, Show, Generic)

makeLenses 'LottoModel

-- Assumptions
-- we will alwayd use the default lotto setup

-- open
-- mintseal
-- play
-- resolve

-- This example will need to use SymValue, SymTxIn etc.

-- As far as I am aware the model only needs to track
  -- The guesses
  -- The total value gambled
  -- The name of the seal
  -- The datum in the TxIn

-- All the references to TxOut in play, open and minted are only used to keep track of the value / the token name of the seal which we should be able to work out in the model.

instance ContractModel LottoModel where
  data Action LottoModel =  Open String String
                          | MintSeal Int
                          | Play String Integer Int
                          | Resolve Int
                          deriving (Eq, Show, Generic)

  initialState = LottoModel { _guesses = []
                             , _secret       = ""
                             , _salt         = ""
                             , _txIn         = Nothing
                             , _token        = Nothing
                             , _value        = 0
                             , _endSlot      = 0
                             , _phase        = Initial
                             }

  nextState a = void $ case a of
    Open scrt slt -> do
      curSlot <- viewModelState currentSlot

      let deadline = toSlotNo . TimeSlot.posixTimeToEnclosingSlot def
                     $ TimeSlot.nominalDiffTimeToPOSIXTime (Lotto.duration def)

      theTxIn <- createTxIn "Lotto txIn"
      txIn        .= Just theTxIn
      value .= 10
      guesses .= []
      withdraw (walletAddr Lotto.organiser) (Lib.ada 10)
      secret .= scrt
      salt .= slt
      endSlot .= curSlot + 5
      phase .= Minting
      wait 1
    MintSeal _ -> do
      theTxIn <- createTxIn "Lotto txIn"
      txIn        .= Just theTxIn
      sealToken <- createToken "Lotto token"
      token .= Just sealToken
      phase .= Playing
      wait 1
    Play g v w -> do
      theTxIn <- createTxIn "Lotto txIn"
      txIn        .= Just theTxIn
      withdraw (walletAddr $ wallet w) (Ada.adaValueOf $ fromInteger v)
      value += v
      guesses %= \ z -> (w , g) : z
      wait 1
    Resolve _ -> do
      -- Fix This: Get margin properly
      vl <- viewContractState value
      sc <- viewContractState secret
      gs <- viewContractState guesses

      let targets = Lib.payGamblers
                Lib.scoreDiffZeros
                vl
                (unsafeRatio 3 100)
                (toBuiltinByteString sc)
                (fixGuesses gs)

      let organiserWinnings = vl - sum (map snd targets)

      deposit (walletAddr Lotto.organiser) (Ada.adaValueOf $ fromInteger organiserWinnings)
      sequence_ [ deposit (walletAddr w) (Ada.adaValueOf $ fromInteger v) | (w, v) <- targets ]

      phase .= Initial
      wait 1

  precondition s a = case a of
    Open secret sale -> currentPhase == Initial
    MintSeal _ -> currentPhase == Minting
    Play g v w -> w /= 4 &&
                  currentPhase == Playing
    Resolve _ -> currentPhase == Resolving
    where currentPhase = s ^. contractState . phase

  arbitraryAction _ = oneof [ Open <$> QC.elements secrets <*> QC.elements salts
                            , MintSeal <$> genWallet
                            , Play <$> QC.elements guessOptions <*> choose @Integer (10, 30) <*> genWallet
                            , Resolve <$> genWallet
                            ]
                  where
                    genWallet = QC.choose (1, length knownWallets)


  nextReactiveState slot = do
    deadline <- viewContractState endSlot
    s <- viewContractState phase
    when ((slot >= deadline) && (s == Playing)) $ do
      phase .= Resolving


  validFailingAction _ _ = False

voidCatch m = catchError (void m) (\ _ -> pure ())

-- | Tell us how to run an `AuctionModel` in the `SuperMockChain` - an
-- extension of the Cooked Validator `MockChain` monad adapted to
-- work with `QuickCheck.ContractModel`.
instance RunModel LottoModel (SuperMockChain ()) where
  -- `perform` runs API actions by calling the off-chain code of
  -- the contract in the `SuperMockChain` monad.

  perform _ (Open s slt) _ = void $ do
    let
      secret = toBuiltinByteString s
      salt = toBuiltinByteString slt
      hashedSecret = Lib.hashSecret secret (Just salt)
    (initLottoRef, initLotto) <- Lotto.open def hashedSecret salt
    registerTxIn "Lotto txIn"  (toTxIn initLottoRef)
  perform s (MintSeal _) translate = void $ do
    let mref = translate <$> s ^. contractState . txIn
        lotto = s ^. contractState . value
        sealPolicy = TScripts.Versioned (Lib.mkMintingPolicy Lotto.script) TScripts.PlutusV2
        currency = L.scriptCurrencySymbol sealPolicy
    (ref, txout, tname) <- Lotto.mintSeal (Plutus.fromCardanoTxIn $ fromJust mref)
                                          (Ada.adaValueOf $ fromInteger lotto)
    registerTxIn "Lotto txIn"  (toTxIn ref)
    registerToken "Lotto token" (toAssetId (assetClass currency tname))
  perform s (Play g v w) translate = void $ do
    let mref  = translate <$> s ^. contractState . txIn
        seal  = translate <$> s ^. contractState . token
        lotto = s ^. contractState . value
        slt   = s ^. contractState . salt
        sealPolicy = TScripts.Versioned (Lib.mkMintingPolicy Lotto.script) TScripts.PlutusV2
        currency = L.scriptCurrencySymbol sealPolicy
        sealName = (getTokenName $ fromJust seal)
        mintVal = LedgerV2.Value $ AssocMap.singleton currency $ AssocMap.singleton sealName 1
    (ref, txout) <- Lotto.play (Plutus.fromCardanoTxIn $ fromJust mref)
                      sealName
                      (mintVal <> (Ada.adaValueOf $ fromInteger lotto))
                      (Lib.hashSecret (toBuiltinByteString g) (Just (toBuiltinByteString slt)))
                      (wallet w)
                      (Ada.adaValueOf $ fromInteger v)
    registerTxIn "Lotto txIn"  (toTxIn ref)
  perform s (Resolve _) translate = void $ do
    let mref  = translate <$> s ^. contractState . txIn
        lotto = s ^. contractState . value
        scrt  = s ^. contractState . secret
        seal  = translate <$> s ^. contractState . token
        sealPolicy = TScripts.Versioned (Lib.mkMintingPolicy Lotto.script) TScripts.PlutusV2
        currency = L.scriptCurrencySymbol sealPolicy
        sealName = (getTokenName $ fromJust seal)
        mintVal = LedgerV2.Value $ AssocMap.singleton currency $ AssocMap.singleton sealName 1
    Lotto.resolve' (toBuiltinByteString scrt)
                   ((Plutus.fromCardanoTxIn $ fromJust mref) ,
                    (mintVal <> (Ada.adaValueOf $ fromInteger lotto)))
                   sealName

  -- we shall not do monitoring yet
  -- monitoring

-- | A standard property that tests that the balance changes
-- predicted by the `ContractModel` instance match the balance changes produced
-- by the `RunModel` instance - up to minimum ada requirements on UTxOs.
prop_Lotto :: Property
prop_Lotto = noShrinking $ QC.withMaxSuccess 100 $ (propRunActions @LottoModel testInit () balanceChangePredicate)


getTokenName :: AssetId -> TokenName
getTokenName (AssetId sym (AssetName tok)) = TokenName (Builtins.toBuiltin tok)

secrets :: [ String ]
secrets = ["bob", "alice", "jane", "steven"]

guessOptions :: [ String ]
guessOptions = ["bob", "alice", "jane", "steven", "john"]

salts :: [ String ]
salts = ["aslkdjs" , "saduenf" , "asjdurnfkli" , "asdlkjasui"]

toBuiltinByteString :: String -> BuiltinByteString
toBuiltinByteString s = LedgerV2.toBuiltin $ T.encodeUtf8 (T.pack s)

toTokenName :: String -> LedgerV2.TokenName
toTokenName s = tokenName $ T.encodeUtf8 (T.pack s)

unitTest1 :: DL LottoModel ()
unitTest1 = do
             action $ Open "jane" "asldkjk"
             action $ MintSeal 4
             action $ Play "bob" 20 8
             -- waitUntilDL 200000
             action $ Resolve 4

prop_Lotto' :: Actions LottoModel -> Property
prop_Lotto' = propRunActions testInit () balanceChangePredicate

propTest :: Property
propTest = withMaxSuccess 1 $ forAllDL unitTest1 prop_Lotto'

fixGuesses :: [(Int, String)] -> [(Wallet, BuiltinByteString)]
fixGuesses xs = map (\ (w , s) -> ((wallet w) , toBuiltinByteString s)) xs
