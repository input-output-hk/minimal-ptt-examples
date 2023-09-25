{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs           #-}
module Plutus.Contract.Test.Cooked.Certification where

import Test.QuickCheck.ContractModel hiding (inv)
import Test.Tasty as Tasty

data Instance c m where
  Instance :: c m => Instance c m

-- | A certification object specifies what tests should be run by the
--   'Plutus.Contract.Test.Certification.Run.certify' function.
data Certification m = Certification {
    certUnitTests          :: Maybe TestTree,    -- ^ Unit tests using "Test.Tasty". See e.g. 'Plutus.Contract.Test.checkPredicateCoverage'.
    certDLTests            :: [(String, DL m ())]                 -- ^ Unit tests using 'Plutus.Contract.Test.ContractModel.DL'.
  }

defaultCertification :: Certification m
defaultCertification = Certification
  {   certUnitTests          = Nothing
    , certDLTests            = []
  }
