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
import Data.ByteString as BS


import Data.Text                  as T
import Data.Text.Encoding         as T


import Cooked.Wallet

import Prelude hiding ((-))
import qualified Plutus.V2.Ledger.Api as LedgerV2

-- | initial distribution s.t. everyone owns five bananas
testInit :: InitialDistribution
testInit = initialDistribution [(i, [Ada.lovelaceValueOf 20_000_000]) | i <- knownWallets]

data LottoModel = LottoModel { _guesses       :: Map Int String
                             -- , _refundSlot    :: L.Slot
                             , _secret        :: String
                             } deriving (Eq, Show, Generic)

makeLenses 'LottoModel

-- Assumptions
-- we will alwayd use the default lotto setup

-- open
-- mintseal
-- play
-- resolve

instance ContractModel LottoModel where
  data Action LottoModel =  Open String String
                          | MintSeal Int -- LedgerV2.TxOutRef LedgerV2.Value
                          | Play --  LedgerV2.TxOutRef
                                 -- LedgerV2.TokenName
                                 -- LedgerV2.Value
                                 String
                                 Int
                                 Int
                          | Resolve String
                                    -- (LedgerV2.TxOutRef, LedgerV2.TxOut)
                                    -- LedgerV2.TokenName
                          deriving (Eq, Show, Generic)


  -- need to do
  initialState = LottoModel { _guesses = Map.empty
                             -- , _refundSlot    = TimeSlot.posixTimeToEnclosingSlot def
                              --                . escrowDeadline
                              --                $ modelParams
                             , _secret       = "secret"
                             }

  nextState a = void $ case a of
    Open secret salt -> do
      -- withdraw (walletAddr $ wallet w) (Ada.adaValueOf $ fromInteger v)
      -- contributions %= Map.insertWith (+) w v
      wait 1
    MintSeal w -> do
      {- -v <- viewContractState $ contributions . at w . to sum -- to fold
      contributions %= Map.delete w
      deposit (walletAddr $ wallet w) (Ada.adaValueOf $ fromInteger v) -}
      wait 1
    Play g w a -> do
      {- -targets <- viewContractState targets
      contribs <- viewContractState contributions
      sequence_ [ deposit (walletAddr (wallet w)) (Ada.adaValueOf $ fromInteger v) | (w, v) <- Map.toList targets ]
      let leftoverValue = sum contribs - sum targets
      deposit (walletAddr $ wallet w) (Ada.adaValueOf $ fromInteger leftoverValue)
      contributions .= Map.empty -}
      wait 1
    Resolve s -> do
      wait 1

  precondition s a = True
{-
  precondition s a = case a of
    Redeem _ -> (s ^. contractState . contributions . to sum) >= (s ^. contractState . targets . to sum)
             && (s ^. currentSlot < toSlotNo (s ^. contractState . refundSlot))
    Refund w -> s ^. currentSlot >= toSlotNo (s ^. contractState . refundSlot)
                && Nothing /= (s ^. contractState . contributions . at w)
    Pay _ v -> s ^. currentSlot < toSlotNo (s ^. contractState . refundSlot)
             && Ada.adaValueOf (fromInteger v) `geq` Ada.toValue L.minAdaTxOutEstimated
    StealRefund _ _ -> False
-}

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

 {-
  perform _ (Pay w v) _ = voidCatch $ do
    pay (typedValidator modelParams) (wallet w) modelParams (Ada.adaValueOf $ fromInteger v)
  perform _ (Redeem w) _ = voidCatch $ do
    redeem (typedValidator modelParams) (wallet w) modelParams
  perform _ (Refund w) _ = voidCatch $ do
    refund (typedValidator modelParams) (wallet w) modelParams
  perform _ (StealRefund w w') _ = voidCatch $ do
    stealRefund (typedValidator modelParams) (wallet w) (wallet w') modelParams
-}

  -- we shall not do monitoring yet
  -- monitoring

-- | A standard property that tests that the balance changes
-- predicted by the `ContractModel` instance match the balance changes produced
-- by the `RunModel` instance - up to minimum ada requirements on UTxOs.
prop_Escrow :: Actions LottoModel -> Property
prop_Escrow = propRunActions testInit () balanceChangePredicate

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
