module Core where

import qualified Data.Map as Map

type Root = ByteString 
type Slot = Int
type Epoch = Int
type ValidatorIndex = Int

data Store = Store
  { time :: Int,
    genesisTime :: Int,
    justifiedCheckpoint :: Checkpoint,
    finalizedCheckpoint :: Checkpoint,
    bestJustifiedCheckpoint :: Checkpoint,
    proposerBoostRoot :: Root,
    equivocatingIndices :: Set ValidatorIndex,
    blocks :: Map Root Block,
    states :: Map Root BeaconState,
    checkpointStates :: Map Checkpoint BeaconState,
    latestMessages :: Map ValidatorIndex LatestMessage
  }

data Block = Block
  { slot :: Slot,
    parentRoot :: Root
  }
  deriving stock (Eq, Show)

data BeaconState = BeaconState
  { stateSlot :: Slot,
    balances :: [Int],
    currentJustifiedCheckpoint :: Checkpoint,
    currentFinalizedCheckpoint :: Checkpoint
  }
  deriving stock (Eq, Show)

data LatestMessage = LatestMessage
  {
    mEpoch :: Epoch,
    mRoot :: Root
  }

data Checkpoint = Checkpoint
  {
    epoch :: Epoch,
    checkpointRoot :: Root
  }
  deriving stock (Eq, Show)

filterBlockTree :: Store -> Root -> Map Root Block -> Maybe BeaconState
filterBlockTree store' blockRoot' blockTree' = do
  let blocksInStore = blocks store'
  let block' = case Map.lookup blockRoot' blocksInStore of
                          Just b -> b
                          Nothing -> error "could not get block"
  let children = Map.keys $ Map.filter (\v -> parentRoot v == blockRoot') blocksInStore
  headState <- Map.lookup blockRoot' (states store')
  let correctJ = correctJustified store' headState
  let correctF = correctFinalized store' headState
  if correctJ && correctF
    then
      return headState
  else 
    Nothing

-- Recursively checks to fetch an ancestor for a specified root, slot pair.
getAncestor :: Store -> Root -> Slot -> Maybe Root
getAncestor store root' slot' = do
  block' <- Map.lookup root' (blocks store)
  if slot block' > slot'
    then getAncestor store (parentRoot block') slot'
  else 
    return root'

getHead :: Store -> [Root] -> Root -> Root
getHead _ [] hd = hd
getHead store' (_:xs) hd =
  getHead store' xs hd

correctJustified :: Store -> BeaconState -> Bool
correctJustified store' state' =
  epoch (justifiedCheckpoint store') == 0 || 
    currentJustifiedCheckpoint state' == justifiedCheckpoint store'

correctFinalized :: Store -> BeaconState -> Bool
correctFinalized store' state' =
  epoch (finalizedCheckpoint store') == 0 || 
    currentFinalizedCheckpoint state' == finalizedCheckpoint store'

















