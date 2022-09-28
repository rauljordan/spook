module Forkchoice where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.List qualified as List

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
type Gwei = Word64
type Head = Root
type Child = Root

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

latestAttestingBalance :: Store -> BeaconState -> Root -> Gwei
latestAttestingBalance store justifiedState root =
  let activeVals = activeValidators justifiedState
      eligibleVals = filter (\(i, _) -> isEligibleForkchoiceIndex store root i) activeVals
      effectiveBalances = map (effectiveBalance . snd) eligibleVals
      totalEffectiveBalances = sum effectiveBalances in
  totalEffectiveBalances

isViableBranch :: Store -> Root -> Map Root Block -> Bool
isViableBranch store root incomingBlocks =
  False

getHead :: Store -> BeaconState -> Head -> [Child] -> Head
getHead _ _ hd [] = hd
getHead store jState hd _ =
  let children' = Map.keys $ Map.filter (`isParent` hd) (blocks store)
      balances' = map (latestAttestingBalance store jState) children'
      candidates = zip children' balances'
      hd' = List.maximumBy (comparing snd) candidates in
  getHead store jState (fst hd') children'

isParent :: Block -> Root -> Bool
isParent blk rt = parentRoot blk == rt

chainHead :: Store -> Root
chainHead store =
  let blks = blocks store
      startRoot = Types.checkpointRoot (justifiedCheckpoint store) in
  startRoot

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

isEligibleForkchoiceIndex :: Store -> Root -> ValidatorIndex -> Bool
isEligibleForkchoiceIndex store incomingRoot index =
  not (Set.member index (equivocatingIndices store)) && isJust isAncestral where
    isAncestral :: Maybe Bool
    isAncestral = do
      msg <- Map.lookup index (messages store)
      let rt = messageRoot msg
      blk <- Map.lookup rt (blocks store)
      let ancestralRoot = ancestor store rt (slot blk)
      Just $ ancestralRoot == incomingRoot

slotToEpoch :: Slot -> Epoch
slotToEpoch s = s `div` slotsPerEpoch
  
slotsSinceGenesis :: Store -> Int
slotsSinceGenesis store =
  let curr = currentTime store in
  let genesis = genesisTime store in
  fromIntegral (curr - genesis) `div` secondsPerSlot



