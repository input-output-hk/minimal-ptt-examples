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
