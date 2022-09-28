module Forkchoice where

import Data.Map qualified as Map
import Data.Set qualified as Set

import ConsensusTypes qualified as Types

intervalsPerSlot :: Int
intervalsPerSlot = 3

secondsPerSlot :: Int
secondsPerSlot = 12

slotsPerEpoch :: Slot
slotsPerEpoch = 32

type Epoch = Word64
type Slot = Word64
type Root = [Word8]
type ValidatorIndex = Word64

data Store = Store
  {
    currentTime :: Word64,
    genesisTime :: Word64,
    messages :: Map ValidatorIndex LatestMessage,
    justifiedCheckpoint :: Types.Checkpoint,
    bestJustifiedCheckpoint :: Types.Checkpoint,
    finalizedCheckpoint :: Types.Checkpoint,
    proposerBoostRoot :: Root,
    equivocatingIndices :: Set ValidatorIndex,
    blocks :: Map Root Block
  }
  deriving stock (Eq, Show)

data LatestMessage = LatestMessage
  {
    messageEpoch :: Epoch,
    messageRoot :: Root
  }
  deriving stock (Eq, Show)

data Block = Block 
  { 
    slot :: Word64,
    proposerIndex :: ValidatorIndex,
    parentRoot :: Root
  }
  deriving stock (Eq, Show)

data BeaconState = BeaconState
  {
    validators :: [Validator],
    stateSlot :: Word64
  }
  deriving stock (Eq, Show)

data Validator = Validator 
  {
    activationEpoch :: Epoch,
    exitEpoch :: Epoch,
    effectiveBalance :: Gwei
  }
  deriving stock (Eq, Show)

type Gwei = Word64
latestAttestingBalance :: Store -> Root -> BeaconState -> Gwei
latestAttestingBalance store root justifiedState =
  let activeVals = activeValidators justifiedState
      eligibleVals = filter (\(i, _) -> isEligibleForkchoiceIndex i store root) activeVals
      effectiveBalances = map (effectiveBalance . snd) eligibleVals
      totalEffectiveBalances = sum effectiveBalances in
  totalEffectiveBalances

ancestor :: Store -> Root -> Slot -> Root
ancestor store r s = case Map.lookup r (blocks store) of
  Nothing -> r
  Just b -> 
    if slot b > s then ancestor store (parentRoot b) s else r

isActiveAtEpoch :: Validator -> Epoch -> Bool
isActiveAtEpoch validator epoch = 
  activationEpoch validator <= epoch && epoch < exitEpoch validator

activeValidators :: BeaconState -> [(ValidatorIndex, Validator)]
activeValidators beaconState =
  let currEpoch = slotToEpoch $ stateSlot beaconState
      valsWithIndex = zip [(0::ValidatorIndex)..] (validators beaconState) in
      filter (\(_, v) -> isActiveAtEpoch v currEpoch) valsWithIndex 

isEligibleForkchoiceIndex :: ValidatorIndex -> Store -> Root -> Bool
isEligibleForkchoiceIndex index store incomingRoot =
  not (Set.member index (equivocatingIndices store))
    &&
    (case Map.lookup index (messages store) of
    Nothing -> False
    Just msg ->
      let rt = messageRoot msg in
      case Map.lookup rt (blocks store) of
        Nothing -> False
        Just blk ->
          let ancestralRoot = ancestor store rt (slot blk) in
          ancestralRoot == incomingRoot)

slotToEpoch :: Slot -> Epoch
slotToEpoch s = s `div` slotsPerEpoch
  
slotsSinceGenesis :: Store -> Int
slotsSinceGenesis store =
  let curr = currentTime store in
  let genesis = genesisTime store in
  fromIntegral (curr - genesis) `div` secondsPerSlot














