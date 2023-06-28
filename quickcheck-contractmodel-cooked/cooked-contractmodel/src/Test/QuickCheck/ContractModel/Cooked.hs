{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE UndecidableInstances #-}
module Test.QuickCheck.ContractModel.Cooked
  ( module Test.QuickCheck.ContractModel.Cooked
  , module Test.QuickCheck.ContractModel.PlutusCompat
  ) where

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Trans.Control
import Control.Lens

import Cardano.Api
import Cardano.Api.Shelley (ProtocolParameters)
import Cardano.Node.Emulator.Params qualified as Plutus

import Cooked.MockChain.BlockChain hiding (awaitSlot, currentSlot)
import Cooked.MockChain.BlockChain qualified as MockChain
import Cooked.MockChain.Direct
import Cooked.Wallet

import PlutusTx.Builtins qualified as Builtins

import Ledger               qualified as Plutus (getPubKeyHash)
import Ledger.Address       qualified as Plutus
import Ledger.CardanoWallet qualified as Plutus

import Wallet.Emulator.Wallet qualified as Plutus

import Test.QuickCheck
import Test.QuickCheck.ContractModel
import Test.QuickCheck.ContractModel.PlutusCompat
import Test.QuickCheck.ContractModel.Internal
import Test.QuickCheck.Monadic
import Test.QuickCheck.StateModel qualified as StateModel

data RuntimeState s = RuntimeState { _userRuntimeState :: s
                                   , _txsReversed      :: [TxInState]
                                   }

makeLenses ''RuntimeState

newtype SuperMockChainT s m a =
  SuperMockChain { runSuperMockChain :: StateT (RuntimeState s) (MockChainT m) a }
  deriving newtype ( Functor, Applicative, Monad, MonadFail
                   , MonadReader MockChainEnv
                   , MonadError MockChainError
                   , MonadBlockChainWithoutValidation
                   , MonadBlockChainBalancing
                   )
type SuperMockChain s = SuperMockChainT s IO

instance Monad m => MonadState s (SuperMockChainT s m) where
  get = SuperMockChain $ use userRuntimeState
  put = SuperMockChain . (userRuntimeState .=)

type instance StateModel.Realized (SuperMockChainT s m) a = StateModel.Realized m a

instance ( DefaultRealized m
         , Monad m
         , HasChainIndex (SuperMockChainT s m)) => IsRunnable (SuperMockChainT s m) where
  awaitSlot slot = SuperMockChain $ void $ MockChain.awaitSlot (fromSlotNo slot)

fromMockChainSt :: MockChainSt -> ChainState
fromMockChainSt MockChainSt{..} =
  ChainState{ slot = toSlotNo mcstCurrentSlot
            , utxo = fromUtxoIndex mcstIndex
            }

instance HasChainIndex (SuperMockChain s) where
  getChainIndex = do
    txReversed <- SuperMockChain $ use txsReversed
    nid <- asks (Plutus.pNetworkId . mceParams)
    pure $ ChainIndex { transactions = reverse txReversed
                      , networkId    = nid
                      }
  getChainState = do
    st <- SuperMockChain $ lift $ get
    pure $ fromMockChainSt st

instance MonadBlockChain (SuperMockChain s) where
  validateTxSkel skel = do
    st <- SuperMockChain $ lift $ get
    tx <- SuperMockChain $ lift $ validateTxSkel skel
    let newTxInState = TxInState{ tx         = fromCardanoTx tx
                                , chainState = fromMockChainSt st
                                , accepted   = True }
    SuperMockChain $ txsReversed %= (newTxInState :)
    pure tx

deriving instance MonadTransControl RunMonad
deriving via AsTrans RunMonad m instance MonadBlockChain m => MonadBlockChain (RunMonad m)
deriving via AsTrans RunMonad m instance MonadBlockChainWithoutValidation m => MonadBlockChainWithoutValidation (RunMonad m)
deriving via AsTrans RunMonad m instance MonadBlockChainBalancing m => MonadBlockChainBalancing (RunMonad m)

runRunSuperMonad :: InitialDistribution -> s -> RunMonad (SuperMockChain s) Property -> Property
runRunSuperMonad initDist s m =
    monadicIO @Property
  $ run
  $ fmap (either (\ e -> counterexample (show e) $ property False) id)
  $ fmap (fmap fst)
  $ runMockChainTFrom initDist
  $ flip evalStateT (RuntimeState s [])
  $ runSuperMockChain
  $ fmap fst
  $ runWriterT
  $ unRunMonad
  $ awaitSlot 1 >> m

walletAddr :: Wallet -> AddressInEra Era
walletAddr (Plutus.toMockWallet -> w)
  | Just hash <- deserialiseFromRawBytes (AsHash AsPaymentKey)
                                         (Builtins.fromBuiltin pkh) =
      shelleyAddressInEra $
      -- TODO: this is all wrong! It needs to be some magic version of testnet
      makeShelleyAddress Plutus.testnet
                         (PaymentCredentialByKey hash)
                         NoStakeAddress
  | otherwise = error $ "Bad wallet: " ++ show w
  where pkh = Plutus.getPubKeyHash $ Plutus.unPaymentPubKeyHash $ Plutus.mockWalletPaymentPubKeyHash w

balanceChangePredicate :: ProtocolParameters -> ContractModelResult state -> Property
balanceChangePredicate ps result =
  let prettyAddr a = maybe (show a) ("wallet "++) $ Plutus.mwPrintAs =<< addressToWallet a
  in assertBalanceChangesMatch (BalanceChangeOptions False signerPaysFees ps prettyAddr) result
  where addressToWallet a = lookup a [ (walletAddr w, w) | w <- knownWallets ]

propRunActions :: RunModel m (SuperMockChain s)
               => InitialDistribution
               -> s
               -> (ProtocolParameters -> ContractModelResult m -> Property)
               -> Actions m
               -> Property
propRunActions testInit initialRunState pred actions = monadic runTestMonad $ do
  res <- runContractModel actions
  pp  <- run $ lift $ asks (Plutus.pProtocolParams . mceParams)
  pure $ pred pp res
  where runTestMonad = runRunSuperMonad testInit initialRunState
