{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs           #-}
module Plutus.Contract.Test.Certification where

import Test.QuickCheck.ContractModel hiding (inv)
import Test.QuickCheck.ContractModel.Cooked
import Test.Tasty as Tasty

data Instance c m where
  Instance :: c m => Instance c m

-- | A certification object specifies what tests should be run by the
--   'Plutus.Contract.Test.Certification.Run.certify' function.
data Certification m = Certification {
   -- certNoLockedFunds      :: Maybe (NoLockedFundsProof m),
   -- certNoLockedFundsLight :: Maybe (NoLockedFundsProofLight m),
   -- certCrashTolerance     :: Maybe (Instance CrashTolerance m),  -- ^ Contract model for testing robustness against off-chain code crashes.
  --  certWhitelist          :: Maybe Whitelist,                    -- ^ List of allowed exceptions from on-chain code. Usually `Just 'defaultWhiteList'`.
    certRunModel     :: Maybe (Instance (RunModel m (SuperMockChain ()))),  -- ^ Contract model for testing robustness against off-chain code crashes.

    certUnitTests          :: Maybe TestTree,    -- ^ Unit tests using "Test.Tasty". See e.g. 'Plutus.Contract.Test.checkPredicateCoverage'.
    certDLTests            :: [(String, DL m ())]                 -- ^ Unit tests using 'Plutus.Contract.Test.ContractModel.DL'.
  }

defaultCertification :: Certification m
defaultCertification = Certification
  { -- certNoLockedFunds      = Nothing
  -- , certNoLockedFundsLight = Nothing
  -- , certCrashTolerance     = Nothing
  -- , certWhitelist          = Just defaultWhitelist
     certRunModel           = Nothing,
     certUnitTests          = Nothing
    , certDLTests            = []
  }
