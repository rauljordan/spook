module ConsensusTypes where

import SSZ

import Data.ByteString qualified as B

type Root = [Word8]
type Pubkey = [Word8]
type Signature = [Word8]

data SignedBeaconBlockHeader = SignedBeaconBlockHeader
  {
    blockHeader :: BeaconBlockHeader,
    headerSignature :: Signature
  }
  deriving stock (Eq, Show)

data BeaconBlockHeader = BeaconBlockHeader
  {
    headerSlot :: Word64,
    proposerIndex :: Word64,
    parentRoot :: Root,
    stateRoot :: Root,
    bodyRoot :: Root
  }
  deriving stock (Eq, Show)

data BeaconBlockBodyBellatrixBlinded = BeaconBlockBodyBellatrixBlinded
  {
    randaoReveal :: Signature,
    eth1Data :: Eth1Data,
    graffifi :: Root,
    proposerSlashings :: [ProposerSlashing],
    attesterSlashings :: [AttesterSlashing],
    attestations :: [Attestation],
    deposits :: [Deposit],
    voluntaryExits :: [SignedVoluntaryExit],
    syncAggregate :: SyncAggregate,
    executionPayloadHeader :: ExecutionPayloadHeader
  }
  deriving stock (Eq, Show)

data Eth1Data = Eth1Data
  { 
    depositRoot :: Root,
    depositCount :: Word64,
    blockHash :: Root
  }
  deriving stock (Eq, Show)

data ProposerSlashing = ProposerSlashing
  { 
    header1 :: SignedBeaconBlockHeader,
    header2 :: SignedBeaconBlockHeader
  }
  deriving stock (Eq, Show)

data AttesterSlashing = AttesterSlashing
  { 
    att1 :: IndexedAttestation,
    att2 :: IndexedAttestation
  }
  deriving stock (Eq, Show)

data IndexedAttestation = IndexedAttestation
  { 
    attestingIndices :: [Word64],
    indexedAttestationData :: AttestationData,
    indexedAttestationSignature :: Signature
  }
  deriving stock (Eq, Show)

data Attestation = Attestation
  { 
    aggregationBits :: ByteString,
    attestationData :: AttestationData,
    signature :: Signature
  }
  deriving stock (Eq, Show)

data AttestationData = AttestationData
  { 
    slot :: Word64,
    committeeIndex :: Word64,
    beaconBlockRoot :: Root,
    source :: Checkpoint,
    target :: Checkpoint
  }
  deriving stock (Eq, Show)

data Checkpoint = Checkpoint
  { 
    epoch :: Word64,
    root :: Root
  }
  deriving stock (Eq, Show)

data Deposit = Deposit
  { 
    proof :: [[Word8]],
    depositData:: DepositData
  }
  deriving stock (Eq, Show)

data DepositData = DepositData
  { 
    depositPubKey :: Pubkey,
    depositWithdrawalCredentials :: Root,
    amount :: Word64,
    depositSignature :: Signature
  }
  deriving stock (Eq, Show)

data SignedVoluntaryExit = SignedVoluntaryExit
  {
    voluntaryExit :: VoluntaryExit,
    voluntaryExitSignature :: Signature
  }
  deriving stock (Eq, Show)

data VoluntaryExit = VoluntaryExit
  { 
    voluntaryExitEpoch :: Word64,
    voluntaryExitValidatorIndex :: Word64
  }
  deriving stock (Eq, Show)

data SyncAggregate = SyncAggregate
  {
    syncCommitteeBits :: ByteString,
    syncCommitteeSignature :: Signature
  }
  deriving stock (Eq, Show)

type Address = [Word8]
type Bloom = [Word8]

data ExecutionPayloadHeader = ExecutionPayloadHeader
  {
    parentHash :: Root,
    feeRecipient :: Address,
    payloadStateRoot :: Root,
    receiptsRoot :: Root,
    logsBloom :: Bloom,
    prevRandao :: Root,
    blockNumber :: Word64,
    gasLimit :: Word64,
    gasUsed :: Word64,
    timestamp :: Word64,
    extraData :: Root,
    baseFeePerGas :: Root,
    payloadBlockHash :: Root,
    transactionsRoot :: Root
  }
  deriving stock (Eq, Show)

blockBodyToContainer :: BeaconBlockBodyBellatrixBlinded -> SSZItem a
blockBodyToContainer item =
  let randao' = map SUint8 (randaoReveal item) in
  let atts' = map attToContainer (attestations item) in
  SContainer [
    SVector 96 randao',
    SList 128 atts'
  ]

emptyAttestation :: Attestation
emptyAttestation = Attestation
  { 
    aggregationBits = B.empty,
    attestationData = emptyAttestationData,
    signature = [0]
  }

attToContainer :: Attestation -> SSZItem a
attToContainer item =
  let sig' = map SUint8 (signature item) in
  SContainer [
    SBitlist (aggregationBits item),
    attestationDataToContainer (attestationData item),
    SVector 96 sig'
  ]

emptyAttestationData :: AttestationData
emptyAttestationData = AttestationData
  { 
    slot = 0,
    committeeIndex = 0,
    beaconBlockRoot = [0],
    source = emptyCheckpoint,
    target = emptyCheckpoint
  }
                                       
attestationDataToContainer :: AttestationData -> SSZItem a
attestationDataToContainer item =
  let root'  = map SUint8 (beaconBlockRoot item) in
  SContainer [
    SUint64 $ slot item,
    SUint64 $ committeeIndex item,
    SVector 32 root',
    checkpointToContainer (source item),
    checkpointToContainer (target item)
  ]

emptyCheckpoint :: Checkpoint
emptyCheckpoint = Checkpoint { epoch = 0, root = [0] }

checkpointToContainer :: Checkpoint -> SSZItem a
checkpointToContainer item =
  let root' = map SUint8 (root item) in
  SContainer [SUint64 $ epoch item, SVector 32 root']

executionPayloadHeaderToContainer :: ExecutionPayloadHeader -> SSZItem a
executionPayloadHeaderToContainer item =
  SContainer [
    SVector 32 (map SUint8 $ parentHash item),
    SVector 20 (map SUint8 $ feeRecipient item),
    SVector 32 (map SUint8 $ payloadStateRoot item),
    SVector 32 (map SUint8 $ receiptsRoot item),
    SVector 256 (map SUint8 $ logsBloom item),
    SVector 32 (map SUint8 $ prevRandao item),
    SUint64 $ blockNumber item,
    SUint64 $ gasLimit item,
    SUint64 $ gasUsed item,
    SUint64 $ timestamp item,
    SList 32 (map SUint8 $ extraData item),
    SVector 32 (map SUint8 $ baseFeePerGas item),
    SVector 32 (map SUint8 $ payloadBlockHash item),
    SVector 32 (map SUint8 $ transactionsRoot item)
  ]

emptyExecutionPayloadHeader :: ExecutionPayloadHeader
emptyExecutionPayloadHeader = ExecutionPayloadHeader
  {
    parentHash = [0],
    feeRecipient = [0],
    payloadStateRoot = [0],
    receiptsRoot = [0],
    logsBloom = [0],
    prevRandao = [0],
    blockNumber = 0,
    gasLimit = 0,
    gasUsed = 0,
    timestamp = 0,
    extraData = [],
    baseFeePerGas = [0],
    payloadBlockHash = [0],
    transactionsRoot = [0]
  }
