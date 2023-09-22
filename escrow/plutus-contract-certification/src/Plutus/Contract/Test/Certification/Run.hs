{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Plutus.Contract.Test.Certification.Run
  ( -- * A certification report holds all the necessary information
    -- to make sense of certification results
    CertificationReport
  , certResJSON
  -- * There are a tonne of lenses
  , certRes_standardPropertyResult
  -- TODO: turn on when double satisfaction is activated again
  -- , certRes_doubleSatisfactionResult
  , certRes_noLockedFundsResult
  , certRes_noLockedFundsLightResult
  , certRes_standardCrashToleranceResult
  , certRes_unitTestResults
  , certRes_coverageReport
  , certRes_whitelistOk
  , certRes_whitelistResult
  , certRes_DLTests
  -- * and we have a function for running certification
  , CertificationOptions(..)
  , CertificationEvent(..)
  , CertificationTask(..)
  , certificationTasks
  , hasQuickCheckTests
  , defaultCertificationOptions
  , certify
  , certifyWithOptions
  , certifyWithOutput
  ) where

import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Exception
import Control.Lens
import Control.Monad.Writer
import Data.Aeson (FromJSON (..), ToJSON (..), encode)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.IntMap qualified as IntMap
import Data.Maybe
import GHC.Generics
import Plutus.Contract.Test.Certification
-- import Plutus.Contract.Test.ContractModel
-- import Plutus.Contract.Test.ContractModel.CrashTolerance
import Test.QuickCheck.ContractModel hiding (inv)
import Test.QuickCheck.ContractModel.Cooked
import System.Random.SplitMix
import Test.QuickCheck as QC
import Test.QuickCheck.Property
import Test.QuickCheck.Random as QC
import Test.Tasty qualified as Tasty
import Test.Tasty.Runners qualified as Tasty
import Text.Read hiding (lift)

import Plutus.Script.Utils.Ada qualified as Ada
import Cooked.Wallet


newtype JSONShowRead a = JSONShowRead a

instance Show a => ToJSON (JSONShowRead a) where
  toJSON (JSONShowRead a) = toJSON (show a)

instance Read a => FromJSON (JSONShowRead a) where
  parseJSON v = do
    str <- parseJSON v
    case readMaybe str of
      Nothing -> fail "JSONShowRead: readMaybe Nothing"
      Just a  -> return $ JSONShowRead a

deriving via (JSONShowRead SMGen) instance FromJSON SMGen
deriving via (JSONShowRead SMGen) instance ToJSON SMGen

deriving via SMGen instance FromJSON QCGen
deriving via SMGen instance ToJSON QCGen
deriving instance Generic QC.Result
deriving instance ToJSON QC.Result
deriving instance FromJSON QC.Result

instance ToJSON SomeException where
  toJSON (SomeException e) = toJSON (show e)
instance FromJSON SomeException where
  parseJSON v = do
    str <- parseJSON v
    return $ SomeException (ErrorCall str)

testInit :: InitialDistribution
testInit = initialDistribution [(i, [Ada.lovelaceValueOf 20_000_000]) | i <- knownWallets]

data TastyResult = Result
  { resultOutcome          :: Tasty.Outcome
  , resultDescription      :: String
  , resultShortDescription :: String
  , resultTime             :: Tasty.Time
  }
  deriving (Generic, ToJSON)

deriving instance Generic Tasty.FailureReason
deriving instance ToJSON Tasty.FailureReason
deriving instance ToJSON Tasty.Outcome

instance ToJSON Tasty.Result where
  toJSON r = toJSON $ Result { resultOutcome          = Tasty.resultOutcome r
                             , resultDescription      = Tasty.resultDescription r
                             , resultShortDescription = Tasty.resultShortDescription r
                             , resultTime             = Tasty.resultTime r
                             }

data CertificationReport m = CertificationReport {
    _certRes_standardPropertyResult       :: QC.Result,
    -- _certRes_doubleSatisfactionResult     :: QC.Result,
    _certRes_unitTestResults              :: [Tasty.Result],
    _certRes_DLTests                      :: [(String, QC.Result)]
  } deriving (Show, Generic, ToJSON)
makeLenses ''CertificationReport

certResJSON :: CertificationReport m -> String
certResJSON = unpack . encode

data CertificationEvent = QuickCheckTestEvent (Maybe Bool)  -- ^ Nothing if discarded, otherwise test result
                        | QuickCheckNumTestsEvent Int
                        | StartCertificationTask CertificationTask
                        | FinishedTask Bool
                        | CertificationDone
  deriving (Eq, Show)

data CertificationTask = UnitTestsTask
                       | StandardPropertyTask
                       -- | DoubleSatisfactionTask
                       | DLTestsTask
  deriving (Eq, Show, Enum, Bounded, Ord)

hasQuickCheckTests :: CertificationTask -> Bool
hasQuickCheckTests t = t /= UnitTestsTask

-- | The list of certification tasks that will be run for a given certification object.
certificationTasks :: Certification m -> [CertificationTask]
certificationTasks Certification{..} = filter run [minBound..maxBound]
  where
    run UnitTestsTask          = isJust certUnitTests
    run StandardPropertyTask   = True
    -- run DoubleSatisfactionTask = True
    run DLTestsTask            = not $ null certDLTests

data CertificationOptions = CertificationOptions { certOptNumTests  :: Int
                                                 , certOptOutput    :: Bool
                                                 , certEventChannel :: Maybe (Chan CertificationEvent)
                                                 }

defaultCertificationOptions :: CertificationOptions
defaultCertificationOptions = CertificationOptions { certOptOutput = True
                                                   , certOptNumTests = 100
                                                   , certEventChannel = Nothing }

type CertMonad = IO

runCertMonad :: CertMonad (CertificationReport m) -> IO (CertificationReport m)
runCertMonad m = do
                  res <- m
                  return res

addOnTestEvents :: Testable prop => CertificationOptions -> prop -> Property
addOnTestEvents opts prop
  | Just ch <- certEventChannel opts = mapResult (addCallback ch) prop
  | otherwise                        = property prop
  where
    addCallback ch r = r { callbacks = cb : callbacks r }
      where cb = PostTest NotCounterexample $ \ _st res -> writeChan ch $ QuickCheckTestEvent (ok res)

runStandardProperty :: forall m. ContractModel m => CertificationOptions -> CertMonad QC.Result
runStandardProperty opts = quickCheckWithResult
                                  (mkQCArgs opts)
                                   $ addOnTestEvents opts $
                                               propRunActions
                                                 @m
                                                 testInit
                                                 ()
                                                 balanceChangePredicate

-- TODO: turn on when double satisfaction is re-implemented
{-
checkDS :: forall m. ContractModel m => CertificationOptions -> CertMonad QC.Result
checkDS opts = quickCheckWithResult
                                   (mkQCArgs opts)

                                 $ \ covopts -> addOnTestEvents opts $
                                                checkDoubleSatisfactionWithOptions
                                                  @m
                                                  defaultCheckOptionsContractModel
                                                  covopts
-}


mkQCArgs :: CertificationOptions -> Args
mkQCArgs CertificationOptions{..} = stdArgs { chatty = certOptOutput , maxSuccess = certOptNumTests }

runUnitTests :: Tasty.TestTree -> CertMonad [Tasty.Result]
runUnitTests t = do
    res <- Tasty.launchTestTree mempty t $ \ status -> do
      rs <- atomically $ mapM waitForDone (IntMap.elems status)
      return $ \ _ -> return rs
    return res
  where
    waitForDone tv = do
      s <- readTVar tv
      case s of
        Tasty.Done r -> return r
        _            -> retry

checkDerived :: forall d m c. (c m => ContractModel (d m))
             => Maybe (Instance c m)
             -> CertificationOptions
             -> CertificationTask
             -> CertMonad (Maybe QC.Result)
checkDerived Nothing _ _               = return Nothing
checkDerived (Just Instance) opts task =
  Just <$> wrapQCTask opts task (runStandardProperty @(d m) opts)


checkDLTests :: forall m. ContractModel m
            => [(String, DL m ())]
            -> CertificationOptions
            -> CertMonad [(String, QC.Result)]
checkDLTests [] _ = pure []
checkDLTests tests opts =
  wrapTask opts DLTestsTask (Prelude.all (QC.isSuccess . snd))
  $ sequence [(s,) <$> (quickCheckWithResult
                                    (mkQCArgs opts)
                                    $ addOnTestEvents opts $
                                        forAllDL dl (propRunActions
                                                      testInit
                                                      ()
                                                      balanceChangePredicate))
             | (s, dl) <- tests ]

startTaskEvent :: CertificationOptions -> CertificationTask -> CertMonad ()
startTaskEvent opts task | Just ch <- certEventChannel opts = liftIO $ writeChan ch $ StartCertificationTask task
                         | otherwise                        = pure ()

finishTaskEvent :: CertificationOptions -> Bool -> CertMonad ()
finishTaskEvent opts res | Just ch <- certEventChannel opts = liftIO $ writeChan ch $ FinishedTask res
                         | otherwise                        = pure ()

numTestsEvent :: CertificationOptions -> CertMonad ()
numTestsEvent opts | Just ch <- certEventChannel opts = liftIO $ writeChan ch $ QuickCheckNumTestsEvent $ certOptNumTests opts
                   | otherwise                        = pure ()

certify :: forall m. ContractModel m => Certification m -> IO (CertificationReport m)
certify = certifyWithOptions defaultCertificationOptions

wrapTask :: CertificationOptions
         -> CertificationTask
         -> (r -> Bool)
         -> CertMonad r
         -> CertMonad r
wrapTask opts task resInterp act = do
  startTaskEvent opts task
  res <- act
  finishTaskEvent opts $ resInterp res
  return res

wrapQCTask :: CertificationOptions
           -> CertificationTask
           -> CertMonad QC.Result
           -> CertMonad QC.Result
wrapQCTask opts task m = wrapTask opts task QC.isSuccess $ numTestsEvent opts >> m

certifyWithOptions :: forall m. ContractModel m
                   => CertificationOptions
                   -> Certification m
                   -> IO (CertificationReport m)
certifyWithOptions opts Certification{..} = runCertMonad $ do
  -- Unit tests
  unitTests    <- wrapTask opts UnitTestsTask (Prelude.all Tasty.resultSuccessful)
                $ fromMaybe [] <$> traverse runUnitTests certUnitTests
  -- Standard property
  qcRes        <- wrapQCTask opts StandardPropertyTask
                $ runStandardProperty @m opts
  -- Double satisfaction
  -- dsRes        <- wrapQCTask opts DoubleSatisfactionTask
  --               $ checkDS @m opts
  -- DL tests
  dlRes        <- checkDLTests @m certDLTests opts
  case certEventChannel opts of
    Just ch -> liftIO $ writeChan ch CertificationDone
    Nothing -> pure ()
  -- Final results
  return $ CertificationReport
            { _certRes_standardPropertyResult       = qcRes
            -- , _certRes_doubleSatisfactionResult     = dsRes
            , _certRes_unitTestResults              = unitTests
            , _certRes_DLTests                      = dlRes }
