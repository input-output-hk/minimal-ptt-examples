{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs           #-}
module Plutus.Contract.Test.Certification where

import Cardano.Node.Emulator.Test
import Cardano.Node.Emulator.Test.NoLockedFunds
import Test.QuickCheck.ContractModel
import PlutusTx.Coverage
import Test.Tasty as Tasty
import Data.IORef

data Instance c m where
  Instance :: c m => Instance c m

-- newtype CoverageRef = CoverageRef (IORef CoverageData)

-- | A certification object specifies what tests should be run by the
--   'Plutus.Contract.Test.Certification.Run.certify' function.
data Certification m = Certification {
    certCoverageIndex      :: CoverageIndex,                      -- ^ Coverage locations for on-chain test coverage.
    certNoLockedFunds      :: Maybe (NoLockedFundsProof m),
    certNoLockedFundsLight :: Maybe (NoLockedFundsProofLight m),
    -- certCrashTolerance     :: Maybe (Instance CrashTolerance m),  -- ^ Contract model for testing robustness against off-chain code crashes.
    -- certWhitelist          :: Maybe Whitelist,                    -- ^ List of allowed exceptions from on-chain code. Usually `Just 'defaultWhiteList'`.
    certUnitTests          :: Maybe ((IORef CoverageData) -> TestTree),    -- ^ Unit tests using "Test.Tasty". See e.g. 'Plutus.Contract.Test.checkPredicateCoverage'.
    certDLTests            :: [(String, DL m ())],                -- ^ Unit tests using 'Plutus.Contract.Test.ContractModel.DL'.
    certCheckOptions       :: Maybe (Options m)                  -- ^ Check options for certification
  }

defaultCertification :: Certification m
defaultCertification = Certification
  { certCoverageIndex      = mempty
  , certNoLockedFunds      = Nothing
  , certNoLockedFundsLight = Nothing
  , certUnitTests          = Nothing
  -- , certCrashTolerance     = Nothing
  -- , certWhitelist          = Just defaultWhitelist
  , certDLTests            = []
  , certCheckOptions       = Nothing
  }
