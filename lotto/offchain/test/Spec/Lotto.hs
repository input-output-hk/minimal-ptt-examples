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

import PlutusTx.Prelude (BuiltinByteString)
import Data.ByteString qualified as BS


import Data.Text qualified as T
import Data.Text.Encoding qualified as T

import qualified Lib
import qualified Lotto


import Cooked.Wallet

import Prelude hiding ((-))
import qualified Plutus.V2.Ledger.Api as LedgerV2

import Ledger.Tx.CardanoAPI qualified as Plutus
import Data.Maybe

-- | initial distribution s.t. everyone owns five bananas
testInit :: InitialDistribution
testInit = initialDistribution [(i, [Ada.lovelaceValueOf 20_000_000]) | i <- knownWallets]

data LottoModel = LottoModel { _guesses       :: Map Int String
                             -- , _refundSlot    :: L.Slot
                             , _secret        :: String
                             , _salt          :: String
                             , _txIn          :: Maybe SymTxIn
                             , _token         :: Maybe SymToken
                             , _value         :: Integer
--                             , _value         :: Maybe SymValue
                             , _tName         :: String
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
                                    -- (LedgerV2.TxOutRef, LedgerV2.TxOut)
                                    -- LedgerV2.TokenName
                          deriving (Eq, Show, Generic)


  -- need to do
  initialState = LottoModel { _guesses = Map.empty
                             -- , _refundSlot    = TimeSlot.posixTimeToEnclosingSlot def
                              --                . escrowDeadline
                              --                $ modelParams
                             , _secret       = ""
                             , _salt         = ""
                             , _txIn         = Nothing
                             , _token        = Nothing
                             , _value        = 0
                             , _tName        = ""
                             , _phase        = Initial
                             }

  nextState a = void $ case a of
    Open secret salt -> do
      theTxIn <- createTxIn "Lotto txIn"
      txIn        .= Just theTxIn
      -- needs to create txin with what is in the datum
      -- the value that is used to open the contract is stored
        -- it is always Lib.ada (10)

      -- withdraw (walletAddr $ wallet w) (Ada.adaValueOf $ fromInteger v)
      -- contributions %= Map.insertWith (+) w v
      phase .= Minting
      wait 1
    MintSeal _ -> do
      theTxIn <- createTxIn "Lotto txIn"
      txIn        .= Just theTxIn
      -- Now _value needs to add the minted value e.g.
        -- LedgerV2.Value $ Map.singleton currency $ Map.singleton sealName 1
      -- Should also store the tokenName of the seal
      -- Datum does not change

      -- toString will change tokenName to string

      {- -v <- viewContractState $ contributions . at w . to sum -- to fold
      contributions %= Map.delete w
      deposit (walletAddr $ wallet w) (Ada.adaValueOf $ fromInteger v) -}
      phase .= Playing
      wait 1
    Play g v w -> do
      theTxIn <- createTxIn "Lotto txIn"
      txIn        .= Just theTxIn
      -- model
        -- add gambled value to the total _value
        -- add guess to _guesses map
      -- datum
        -- update datum with player guess
        -- can be don with Data.addplayer

      {- -targets <- viewContractState targets
      contribs <- viewContractState contributions
      sequence_ [ deposit (walletAddr (wallet w)) (Ada.adaValueOf $ fromInteger v) | (w, v) <- Map.toList targets ]
      let leftoverValue = sum contribs - sum targets
      deposit (walletAddr $ wallet w) (Ada.adaValueOf $ fromInteger leftoverValue)
      contributions .= Map.empty -}
      wait 1
    Resolve _ -> do
      -- will look into later
      wait 1

  precondition s a = case a of
    Open secret sale -> currentPhase == Initial
    MintSeal _ -> currentPhase == Minting
    Play g v w -> currentPhase == Playing
    Resolve _ -> currentPhase == Resolving
    where currentPhase = s ^. contractState . phase

  arbitraryAction _ = oneof [ Open <$> QC.elements secrets <*> QC.elements salts
                            , MintSeal <$> genWallet
                            , Play <$> QC.elements guessOptions <*> choose @Integer (10, 30) <*> genWallet
                            , Resolve <$> genWallet
                            ]
                  where
                    genWallet = QC.choose (1, length knownWallets)


  -- negative testing is off for now
  validFailingAction _ _ = False

{-
  arbitraryAction _ = oneof [ Pay <$> genWallet <*> choose @Integer (10, 30)
                            , Redeem <$> genWallet
                            , Refund <$> genWallet
                            , StealRefund <$> genWallet <*> genWallet
                            ]
                  where
                    genWallet = QC.choose (1, length knownWallets)
-}

voidCatch m = catchError (void m) (\ _ -> pure ())

-- | Tell us how to run an `AuctionModel` in the `SuperMockChain` - an
-- extension of the Cooked Validator `MockChain` monad adapted to
-- work with `QuickCheck.ContractModel`.
instance RunModel LottoModel (SuperMockChain ()) where
  -- `perform` runs API actions by calling the off-chain code of
  -- the contract in the `SuperMockChain` monad.

  perform _ (Open s slt) _ = voidCatch $ do
    let
      secret = toBuiltinByteString s
      salt = toBuiltinByteString slt
      hashedSecret = Lib.hashSecret secret (Just salt)
    (initLottoRef, initLotto) <- Lotto.open def hashedSecret salt
    registerTxIn "Lotto txIn"  (toTxIn initLottoRef)
  perform s (MintSeal _) translate = voidCatch $ do
    let mref = translate <$> s ^. contractState . txIn
        lotto = s ^. contractState . value
    (ref, txout, tname) <- Lotto.mintSeal (Plutus.fromCardanoTxIn $ fromJust mref)
                                          (Ada.adaValueOf $ fromInteger lotto)
    registerTxIn "Lotto txIn"  (toTxIn ref)
  perform s (Play g v w) translate = voidCatch $ do
    let mref  = translate <$> s ^. contractState . txIn
        seal  = s ^. contractState . tName
        lotto = s ^. contractState . value
        slt   = s ^. contractState . salt
    (ref, txout) <- Lotto.play (Plutus.fromCardanoTxIn $ fromJust mref)
                      (toTokenName seal)
                      (Ada.adaValueOf $ fromInteger lotto)
                      (Lib.hashSecret (toBuiltinByteString g) (Just (toBuiltinByteString slt)))
                      (wallet w)
                      (Ada.adaValueOf $ fromInteger v)
    registerTxIn "Lotto txIn"  (toTxIn ref)
  perform s (Resolve _) translate = voidCatch $ do
    let mref  = translate <$> s ^. contractState . txIn
        lotto = s ^. contractState . value
        scrt  = s ^. contractState . secret
        seal  = s ^. contractState . tName
    Lotto.resolve' (toBuiltinByteString scrt)
                   ((Plutus.fromCardanoTxIn $ fromJust mref) , (Ada.adaValueOf $ fromInteger lotto))
                   (toTokenName seal)

  -- we shall not do monitoring yet
  -- monitoring

-- | A standard property that tests that the balance changes
-- predicted by the `ContractModel` instance match the balance changes produced
-- by the `RunModel` instance - up to minimum ada requirements on UTxOs.
prop_Lotto :: Actions LottoModel -> Property
prop_Lotto = propRunActions testInit () balanceChangePredicate


secrets :: [ String ]
secrets = ["bob", "alice", "jane", "steven"]

guessOptions :: [ String ]
guessOptions = ["bob", "alice", "jane", "steven", "john"]

salts :: [ String ]
salts = ["aslkdjs" , "saduenf" , "asjdurnfkli" , "asdlkjasui"]

-- Actions
-- open
-- mintseal
-- play
-- resolve


-- What does a manyWalletPlay do

-- Create secret from salt
-- Lotto.open
-- Lotto.mintSeal
-- Some number of plays until deadline
-- administrator resolve play

{-
The Administrator Creates a UTxO of the Lotto
Output UTxOs:
The lotto with some datum. At this point, that could be any datum but people won’t play until one calls Initialise.
The Administrator Initialises the Game (redeemer Initialise)
Only allowed for the administrator.

Input UTxOs:
Any lotto, hereafter called “the” lotto.
Output UTxOs:
The lotto with a seal that identifies the instance uniquely and proves that it has been created in a sensible way.
A Gambler Plays (redeemer Play)
Only available before deadline.

Input UTxOs:
The lotto.
The player carrying enough money.
Output UTxOs:
The lotto with (at least bidAmount) more money and an additional entry in players carrying the signatory’s PubKeyHash as well as a word, i.e. some byte String of the player’s choice.
The Administrator Resolves the Game (redeemer Resolve)
Only available after deadline.

Input UTxOs:
The lotto.
Output UTxOs:
The administrator getting the leftover money.
Outputs dispatching the money among the winners (see Winning).
-}

toBuiltinByteString :: String -> BuiltinByteString
toBuiltinByteString s = LedgerV2.toBuiltin $ T.encodeUtf8 (T.pack s)

toTokenName :: String -> LedgerV2.TokenName
toTokenName s = tokenName $ T.encodeUtf8 (T.pack s)

{-
  open produces --  m (LedgerV2.TxOutRef, LedgerV2.TxOut)

  mint produces -- -- | The new (authentic) lotto and the name of the seal.
    (authenticatedLottoRef, authenticatedLotto, seal) <-
  m (LedgerV2.TxOutRef, LedgerV2.TxOut, LedgerV2.TokenName)
-}
