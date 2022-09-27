module Forkchoice where

import Data.Map

import ConsensusTypes qualified as Types

intervalsPerSlot :: Int
intervalsPerSlot = 3

secondsPerSlot :: Int
secondsPerSlot = 12

type Epoch = Word64
type Slot = Word64
type Root = [Word8]
type ValidatorIndex = Word64

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

data Validator = Validator 
  {
    activationEpoch :: Epoch,
    exitEpoch :: Epoch
  }
  deriving stock (Eq, Show)

data Store = Store
  {
    currentTime :: Word64,
    genesisTime :: Word64,
    messages :: [LatestMessage],
    justifiedCheckpoint :: Types.Checkpoint,
    bestJustifiedCheckpoint :: Types.Checkpoint,
    finalizedCheckpoint :: Types.Checkpoint,
    proposerBoostRoot :: Root,
    equivocatingIndices :: Set [ValidatorIndex],
    blocks :: Map Root Block
  }
  deriving stock (Eq, Show)
  
slotsSinceGenesis :: Store -> Int
slotsSinceGenesis store =
  let curr = currentTime store in
  let genesis = genesisTime store in
  fromIntegral (curr - genesis) `div` secondsPerSlot

ancestor :: Store -> Root -> Slot -> Root
ancestor store r s = case lookup r (blocks store) of
  Nothing -> r
  Just b -> 
    if slot b > s then ancestor store (parentRoot b) s else r

type Gwei = Word64
latestAttestingBalance :: Maybe Gwei
latestAttestingBalance =
  Nothing

isActiveAtEpoch :: Validator -> Epoch -> Bool
isActiveAtEpoch validator epoch = 
  activationEpoch validator <= epoch && epoch < exitEpoch validator

























