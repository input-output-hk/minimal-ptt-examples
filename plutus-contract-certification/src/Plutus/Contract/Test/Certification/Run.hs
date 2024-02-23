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
  , CertOptNumTests(..)
  , CertificationEvent(..)
  , CertificationTask(..)
  , certificationTasks
  , hasQuickCheckTests
  , defaultCertificationOptions
  , defaultCertOptNumTests
  , certify
  , certifyWithOptions
  , certifyWithCheckOptions
  , genCoverageFile
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

{-
import Plutus.Contract.Test ( CheckOptions )
import Plutus.Contract.Test.Certification
import Plutus.Contract.Test.ContractModel
import Plutus.Contract.Test.ContractModel.CrashTolerance
-}

import Plutus.Contract.Test.Certification
import Cardano.Node.Emulator.Test
import Cardano.Node.Emulator.Test.Coverage
import Cardano.Node.Emulator.Test.NoLockedFunds
import Test.QuickCheck.ContractModel
import Data.IORef


import PlutusTx.Coverage
import System.Random.SplitMix
import Test.QuickCheck as QC
import Test.QuickCheck.Property
import Test.QuickCheck.Random as QC
import Test.Tasty qualified as Tasty
import Test.Tasty.Runners qualified as Tasty
import Text.Read hiding (lift)

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
    -- TODO: turn on again later
    -- _certRes_doubleSatisfactionResult     :: QC.Result,
    _certRes_noLockedFundsResult          :: Maybe QC.Result,
    _certRes_noLockedFundsLightResult     :: Maybe QC.Result,
    _certRes_standardCrashToleranceResult :: Maybe QC.Result,
    _certRes_unitTestResults              :: [Tasty.Result],
    _certRes_coverageReport               :: CoverageReport,
    _certRes_whitelistOk                  :: Maybe Bool,
    _certRes_whitelistResult              :: Maybe QC.Result,
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
                       | NoLockedFundsTask
                       | NoLockedFundsLightTask
                       | CrashToleranceTask
                       | WhitelistTask
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
    run NoLockedFundsTask      = isJust certNoLockedFunds
    run NoLockedFundsLightTask = isJust certNoLockedFundsLight
    -- run CrashToleranceTask     = isJust certCrashTolerance
    -- run WhitelistTask          = isJust certWhitelist
    run DLTestsTask            = not $ null certDLTests

data CertificationOptions = CertificationOptions { certOptNumTests  :: Int
                                                 , certOptOutput    :: Bool
                                                 , certEventChannel :: Maybe (Chan CertificationEvent)
                                                 }


data CertOptNumTests = CertOptNumTests { numStandardProperty   :: Int
                                       , numNoLockedFunds      :: Int
                                       , numNoLockedFundsLight :: Int
                                       , numCrashTolerance     :: Int
                                       , numWhiteList          :: Int
                                       , numDLTests            :: Int
                                       }

defaultCertOptNumTests :: CertOptNumTests
defaultCertOptNumTests = CertOptNumTests { numStandardProperty   = 100
                                         , numNoLockedFunds      = 100
                                         , numNoLockedFundsLight = 100
                                         , numCrashTolerance     = 100
                                         , numWhiteList          = 100
                                         , numDLTests            = 100
                                         }

defaultCertificationOptions :: CertificationOptions
defaultCertificationOptions = CertificationOptions { certOptOutput = True
                                                   , certOptNumTests = 100
                                                   , certEventChannel = Nothing }

updateCertificationOptions :: CertificationOptions -> Int -> CertificationOptions
updateCertificationOptions CertificationOptions{..} numTests =
                                        CertificationOptions { certOptNumTests = numTests
                                                             , certOptOutput = certOptOutput
                                                             , certEventChannel = certEventChannel
                                                             }

type CertMonad = WriterT CoverageReport IO

liftIORep :: IO (CoverageReport, a) -> CertMonad a
liftIORep io = do
  (rep, a) <- lift io
  tell rep
  return a

runCertMonad :: CertMonad (CertificationReport m) -> IO (CertificationReport m)
runCertMonad m = do
  (rep, cov) <- runWriterT m
  return $ rep & certRes_coverageReport %~ (<> cov)

addOnTestEvents :: Testable prop => CertificationOptions -> prop -> Property
addOnTestEvents opts prop
  | Just ch <- certEventChannel opts = mapResult (addCallback ch) prop
  | otherwise                        = property prop
  where
    addCallback ch r = r { callbacks = cb : callbacks r }
      where cb = PostTest NotCounterexample $ \ _st res -> writeChan ch $ QuickCheckTestEvent (ok res)

runStandardProperty :: forall m. ContractModel m => CertificationOptions -> CoverageIndex -> (Options m) ->  CertMonad QC.Result
runStandardProperty opts covIdx copts = liftIORep $ quickCheckWithCoverageAndResult
                                  (mkQCArgs opts)
                                  (set coverageIndex covIdx defaultCoverageOptions)
                                $ \ covopts -> addOnTestEvents opts $
                                               propRunActionsWithOptions
                                                 @m
                                                 copts
                                                 covopts
                                                 (\ _ -> pure True)

-- TODO: turn on when double satisfaction is re-implemented
-- checkDS :: forall m. ContractModel m => CertificationOptions -> CoverageIndex -> (Options m) -> CertMonad QC.Result
-- checkDS opts covIdx copts = liftIORep $ quickCheckWithCoverageAndResult
--                                   (mkQCArgs opts)
--                                   (set coverageIndex covIdx defaultCoverageOptions)
--                                 $ \ covopts -> addOnTestEvents opts $
--                                                checkDoubleSatisfactionWithOptions
--                                                  @m
--                                                  copts
--                                                  covopts

checkNoLockedFunds :: ContractModel m => CertificationOptions -> (Options m) -> NoLockedFundsProof m -> CertMonad QC.Result
checkNoLockedFunds opts copts prf = lift $ quickCheckWithResult
                                       (mkQCArgs opts)
                                       $ addOnTestEvents opts $ checkNoLockedFundsProofWithOptions copts prf

checkNoLockedFundsLight :: ContractModel m => CertificationOptions -> NoLockedFundsProofLight m -> CertMonad QC.Result
checkNoLockedFundsLight opts prf =
  lift $ quickCheckWithResult
          (mkQCArgs opts)
          $ addOnTestEvents opts $ checkNoLockedFundsProofLight prf

mkQCArgs :: CertificationOptions -> Args
mkQCArgs CertificationOptions{..} = stdArgs { chatty = certOptOutput , maxSuccess = certOptNumTests }

runUnitTests :: ((IORef CoverageData) -> Tasty.TestTree) -> CertMonad [Tasty.Result]
runUnitTests t = liftIORep $ do
    ref <- newIORef mempty -- newCoverageRef
    res <- Tasty.launchTestTree mempty (t ref) $ \ status -> do
      rs <- atomically $ mapM waitForDone (IntMap.elems status)
      return $ \ _ -> return rs
    cov <- readIORef ref -- readCoverageRef ref
    return (CoverageReport mempty cov, res)
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
             -> CoverageIndex
             -> (Options m)
             -> CertMonad (Maybe QC.Result)
checkDerived Nothing _ _ _ _                  = return Nothing
checkDerived (Just Instance) opts task covIdx copts =
  Just <$> wrapQCTask opts task (runStandardProperty @(d m) opts covIdx copts)

{-
checkWhitelist :: forall m. ContractModel m
               => Maybe Whitelist
               -> CertificationOptions
               -> CoverageIndex
               -> (Options m)
               -> CertMonad (Maybe QC.Result)
checkWhitelist Nothing _ _ _         = return Nothing
checkWhitelist (Just wl) opts covIdx copts = do
  a <- wrapQCTask opts WhitelistTask
     $ liftIORep $ quickCheckWithCoverageAndResult
                  (mkQCArgs opts)
                  (set coverageIndex covIdx defaultCoverageOptions)
                  $ \ covopts -> addOnTestEvents opts $
                                 checkErrorWhitelistWithOptions @m
                                    copts
                                    covopts wl
  return (Just a)
-}

checkDLTests :: forall m. ContractModel m
            => [(String, DL m ())]
            -> CertificationOptions
            -> CoverageIndex
            -> (Options m)
            -> CertMonad [(String, QC.Result)]
checkDLTests [] _ _ _ = pure []
checkDLTests tests opts covIdx copts =
  wrapTask opts DLTestsTask (Prelude.all (QC.isSuccess . snd))
  $ sequence [(s,) <$> liftIORep (quickCheckWithCoverageAndResult
                                    (mkQCArgs opts)
                                    (set coverageIndex covIdx defaultCoverageOptions)
                                    $ \ covopts ->
                                        addOnTestEvents opts $
                                        forAllDL dl (propRunActionsWithOptions
                                                      @m
                                                      copts
                                                      covopts (const $ pure True)))
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
certify m = certifyWithOptions defaultCertificationOptions defaultCertOptNumTests m

certifyWithCheckOptions :: forall m. ContractModel m
                        => CertificationOptions
                        -> Certification m
                        -> CertOptNumTests
                        -> IO (CertificationReport m)
certifyWithCheckOptions opts m optNumTest = case certCheckOptions m of
                              Nothing -> certifyWithOptions opts optNumTest m
                              Just copts -> certifyWithOptions' opts optNumTest m copts

genCoverageFile :: IO (CertificationReport m) -> IO (CertificationReport m)
genCoverageFile report = do
                           c <- report
                           writeCoverageReport "coverageReport"  (_certRes_coverageReport c)
                           return c

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
                   -> CertOptNumTests
                   -> Certification m
                   -> IO (CertificationReport m)
certifyWithOptions opts numTests m = certifyWithOptions' opts numTests m defaultOptions -- defaultCheckOptionsContractModel

certifyWithOptions' :: forall m. ContractModel m
                   => CertificationOptions
                   -> CertOptNumTests
                   -> Certification m
                   -> (Options m)
                   -> IO (CertificationReport m)
certifyWithOptions' opts CertOptNumTests{..} Certification{..} copts = runCertMonad $ do
  -- Unit tests
  unitTests    <- wrapTask opts UnitTestsTask (Prelude.all Tasty.resultSuccessful)
                $ fromMaybe [] <$> traverse runUnitTests certUnitTests
  -- Standard property
  qcRes        <- wrapQCTask opts StandardPropertyTask
                $ runStandardProperty @m (updateCertificationOptions opts numStandardProperty) certCoverageIndex copts
  -- TODO: fixme when double sat done
  -- Double satisfaction
  -- dsRes        <- wrapQCTask opts DoubleSatisfactionTask
  --               $ checkDS @m opts certCoverageIndex copts
  -- No locked funds
  noLock       <- traverse (wrapQCTask (updateCertificationOptions opts numNoLockedFunds) NoLockedFundsTask . checkNoLockedFunds (updateCertificationOptions opts numNoLockedFunds) copts)
                           certNoLockedFunds
  -- No locked funds light
  noLockLight  <- traverse (wrapQCTask (updateCertificationOptions opts numNoLockedFundsLight) NoLockedFundsLightTask . checkNoLockedFundsLight (updateCertificationOptions opts numNoLockedFundsLight))
                           certNoLockedFundsLight
  -- Crash tolerance
  -- ctRes        <- checkDerived @WithCrashTolerance certCrashTolerance (updateCertificationOptions opts numCrashTolerance) CrashToleranceTask certCoverageIndex copts
  -- Whitelist
  -- wlRes        <- checkWhitelist @m certWhitelist (updateCertificationOptions opts numWhiteList) certCoverageIndex copts
  -- DL tests
  dlRes        <- checkDLTests @m certDLTests (updateCertificationOptions opts numDLTests) certCoverageIndex copts
  case certEventChannel opts of
    Just ch -> liftIO $ writeChan ch CertificationDone
    Nothing -> pure ()
  -- Final results
  return $ CertificationReport
            { _certRes_standardPropertyResult       = qcRes
            -- , _certRes_doubleSatisfactionResult     = dsRes
            -- , _certRes_standardCrashToleranceResult = ctRes
            , _certRes_noLockedFundsResult          = noLock
            , _certRes_noLockedFundsLightResult     = noLockLight
            , _certRes_unitTestResults              = unitTests
            , _certRes_coverageReport               = CoverageReport certCoverageIndex mempty
            -- , _certRes_whitelistOk                  = whitelistOk <$> certWhitelist
            -- , _certRes_whitelistResult              = wlRes
            , _certRes_DLTests                      = dlRes }
