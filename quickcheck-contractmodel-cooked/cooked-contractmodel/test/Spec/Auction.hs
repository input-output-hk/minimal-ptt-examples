module Spec.Auction where

import Control.Lens hiding (elements)
import Control.Monad.Reader

import Data.Default
import Data.Fixed (Micro)
import Data.Maybe

import GHC.Generics

import Cardano.Api
import Cardano.Node.Emulator.TimeSlot qualified as Plutus

import Ledger.Tx.CardanoAPI qualified as Plutus

import Plutus.Script.Utils.Ada qualified as Plutus
import Plutus.V1.Ledger.Value qualified as Plutus hiding (adaSymbol, adaToken)

import Test.QuickCheck
import Test.QuickCheck.ContractModel
import Test.QuickCheck.ContractModel.Cooked
import Test.QuickCheck.ContractModel.ThreatModel
import Test.QuickCheck.ContractModel.ThreatModel.DoubleSatisfaction
import Test.Tasty
import Test.Tasty.QuickCheck

import Cooked.Currencies
import Cooked.Wallet

import Auction.Offchain qualified as Auction

-- | Value representing a number of bananas
banana :: Integer -> Plutus.Value
banana = Plutus.assetClassValue $ permanentAssetClass "Banana"

-- | initial distribution s.t. wallet 1 has infinity bananas
testInit :: InitialDistribution
testInit = initialDistribution [(w, [Plutus.lovelaceValueOf 20_000_000_000_000_000]) | w <- knownWallets]
        <> initialDistribution [(wallet 1, [banana 5_000_000_000])]

-- | Define a model state for the auction contract.
data AuctionModel = AuctionModel
  { _auctionPhase :: AuctionPhase
  , _deadline     :: SlotNo
  , _currentBid   :: Micro
  , _winner       :: Int
  , _txIn         :: Maybe SymTxIn
  } deriving (Eq, Show, Generic)

-- | The phases of the auction. Note that these are ordered such that
-- `p1 < p2` means that `p1` happens before `p2` in the auction.
data AuctionPhase = None | Offered | NoBid | Bidded deriving (Ord, Eq, Show, Generic)

makeLenses 'AuctionModel

-- This class sets up the model of the auction contract without reference
-- to how running the contract. It's the _model state_ that is a member of
-- the class.
instance ContractModel AuctionModel where

  -- Define the actions (API) of the Auction contract
  data Action AuctionModel = Offer | SetDeadline SlotNo | Bid Int Micro | Hammer | WaitUntilDeadline
                           deriving (Eq, Show, Generic)

  -- Define a generator for actions. Because a lot of the actions
  -- of this contract simply crash if we use them in the
  -- wrong auction state we don't even generate them unless
  -- they are meaningful.
  arbitraryAction s = frequency $
    [ (1,  pure Offer)                   | state == None    ] ++
    [ (1,  SetDeadline <$> genDeadline)  | state == Offered ] ++
    [ (10, Bid <$> genWallet <*> genBid) | state >= NoBid   ] ++
    [ (1,  pure Hammer)                  | state >= Offered ] ++
    [ (1,  pure WaitUntilDeadline)                          ]
    where
      bid   = s ^.contractState . currentBid
      state = s ^. contractState . auctionPhase

      -- Set the deadline some time in the future
      genDeadline = (s ^. currentSlot +) <$> choose (0, 200)
      genBid      = (bid +) <$> choose (1, 100)
      genWallet   = choose (1, length knownWallets)

  -- Define the initial model state
  initialState =
    AuctionModel { _auctionPhase = None
                 , _deadline     = 0
                 , _currentBid   = 0
                 , _winner       = 0
                 , _txIn        = Nothing
                 }

  -- Describe how an action (API call) moves the model state forward.
  -- This includes changes to the blockchain state via `withdraw`, `deposit`, `wait`, etc.
  nextState Offer = do
    -- Set up the auction in the name of wallet 1
    withdraw (walletAddr $ wallet 1) (banana 1)
    theTxIn <- createTxIn "auction txIn"
    auctionPhase .= Offered
    winner       .= 1
    txIn        .= Just theTxIn
    wait 1
  nextState Hammer = do
    use auctionPhase >>= \case
      None -> assertSpec "Impossible!" False
      Bidded -> do
        bid <- use currentBid
        -- Give the auction owner `wallet 1` the value of the
        -- largest bid
        deposit (walletAddr $ wallet 1) (Plutus.adaValueOf bid)
      _ -> pure ()
    -- Give the token we are bidding for to the winner (note that if
    -- no bid has been placed the winner is `wallet 1` as per the
    -- definition of `nextState Offered`).
    w <- use winner
    deposit (walletAddr $ wallet w) (banana 1)
    -- Update the state
    auctionPhase .= None
    deadline     .= 0
    winner       .= 0
    wait 1
  nextState (Bid w newBid) = do
    -- If a bid has been placed, refund the previous bidder their bid.
    -- Note that `newBid` being larger than `currentBid` is a precondition
    -- of this action.
    use auctionPhase >>= \case
      NoBid -> pure ()
      Bidded -> do
        oldBid <- use currentBid
        oldWinner <- use winner
        deposit (walletAddr $ wallet oldWinner) (Plutus.adaValueOf oldBid)
      _ -> assertSpec "Impossible!" False
    -- Place the bid
    withdraw (walletAddr $ wallet w) (Plutus.adaValueOf newBid)
    -- Update the contract state
    currentBid   .= newBid
    winner       .= w
    auctionPhase .= Bidded
    wait 1
  nextState (SetDeadline d) = do
    auctionPhase .= NoBid
    deadline     .= d
    wait 1
  nextState WaitUntilDeadline = do
    d <- use deadline
    waitUntil d

  -- The precondition for each action - if this isn't true the action will
  -- crash at runtime.
  precondition s action = case action of
    Offer -> state == None
    Hammer
      | state > Offered -> slot >= contractDeadline
      | otherwise       -> state == Offered
    Bid _ b
      | slot < contractDeadline
      , state > Offered ->
        if state == NoBid
        then b >= 30
        else b > s ^. contractState . currentBid
      | otherwise -> False
    SetDeadline slt ->
      state == Offered && slot <= slt
    WaitUntilDeadline -> True
    where
      state            = s ^. contractState . auctionPhase
      contractDeadline = s ^. contractState . deadline
      slot             = s ^. currentSlot

  -- A shrinker for actions - helps make counterexamples easier to read.
  shrinkAction _ (Bid w b) = [ Bid w' b' | (w', b') <- shrink (w, b), w' > 0 ]
  shrinkAction _ (SetDeadline (SlotNo slt)) = [ SetDeadline (SlotNo slt') | slt' <- shrink slt ]
  shrinkAction _ _ = []


-- | Tell us how to run an `AuctionModel` in the `SuperMockChain` - an
-- extension of the Cooked Validator `MockChain` monad adapted to
-- work with `QuickCheck.ContractModel`.
instance RunModel AuctionModel (SuperMockChain ()) where
  -- `perform` runs API actions by calling the off-chain code of
  -- the contract in the `SuperMockChain` monad.
  perform _ Offer _ = void $ do
    ref <- Auction.txOffer (wallet 1) (banana 1) 30_000_000
    registerTxIn "auction txIn" (toTxIn ref)
  perform s Hammer translate = void $ do
    let mref = translate <$> s ^. contractState . txIn
    Auction.txHammer (wallet 1) (Plutus.fromCardanoTxIn $ fromJust mref)
  perform s (Bid w b) translate = void $ do
    let mref = translate <$> s ^. contractState . txIn
    Auction.txBid (wallet w) (Plutus.fromCardanoTxIn $ fromJust mref)
                             (Plutus.getLovelace $ Plutus.adaOf b)
  perform s (SetDeadline d) translate = void $ do
    let mref = translate <$> s ^. contractState . txIn
    Auction.txSetDeadline (wallet 1) (Plutus.fromCardanoTxIn $ fromJust mref)
                                     (Plutus.slotToBeginPOSIXTime def $ fromSlotNo d)
  perform s WaitUntilDeadline _ = void $ do
    awaitSlot (s ^. contractState . deadline)

  -- `monitoring` gives us a way to apply `QuickCheck` monitoring
  -- functions like `classify` and `tabulate` to our property to
  -- get a better idea of the test case distribution. In this case
  -- we just track how many tests actually contain a `Hammer` action
  -- indicating that an auction has been finished.
  monitoring _ Hammer _ _ = classify True "Hammered"
  monitoring _ _      _ _ = id

-- | A standard property that tests that the balance changes
-- predicted by the `ContractModel` instance match the balance changes produced
-- by the `RunModel` instance - up to minimum ada requirements on UTxOs.
prop_auction :: Actions AuctionModel -> Property
prop_auction = propRunActions testInit () balanceChangePredicate

-- | A standard property that runs the `doubleSatisfaction` threat
-- model against the auction contract to check for double satisfaction
-- vulnerabilities.
prop_doubleSatisfaction :: Actions AuctionModel -> Property
prop_doubleSatisfaction = propRunActions testInit () (assertThreatModel doubleSatisfaction)

tests :: TestTree
tests = testGroup "Auction"
  [ testProperty "prop_auction" prop_auction
  , testProperty "prop_doubleSatisfaction fails" $ expectFailure prop_doubleSatisfaction
  ]
