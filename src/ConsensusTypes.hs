module ConsensusTypes where

import SSZ

import Data.ByteString qualified as B

type Root = [Word8]
type Pubkey = [Word8]
type Signature = [Word8]
type Address = [Word8]
type Bloom = [Word8]

data SignedBeaconBlockHeader = SignedBeaconBlockHeader
  { blockHeader :: BeaconBlockHeader
  , headerSignature :: Signature
  }
  deriving stock (Eq, Show)

data BeaconBlockHeader = BeaconBlockHeader
  { headerSlot :: Word64
  , proposerIndex :: Word64
  , parentRoot :: Root
  , stateRoot :: Root
  , bodyRoot :: Root
  }
  deriving stock (Eq, Show)

data BeaconBlockBodyBellatrixBlinded = BeaconBlockBodyBellatrixBlinded
  { randaoReveal :: Signature
  , eth1Data :: Eth1Data
  , graffiti :: Root
  , proposerSlashings :: [ProposerSlashing]
  , attesterSlashings :: [AttesterSlashing]
  , attestations :: [Attestation]
  , deposits :: [Deposit]
  , voluntaryExits :: [SignedVoluntaryExit]
  , syncAggregate :: SyncAggregate
  , executionPayloadHeader :: ExecutionPayloadHeader
  }
  deriving stock (Eq, Show)

data Eth1Data = Eth1Data
  { depositRoot :: Root
  , depositCount :: Word64
  , blockHash :: Root
  }
  deriving stock (Eq, Show)

data ProposerSlashing = ProposerSlashing
  { header1 :: SignedBeaconBlockHeader
  , header2 :: SignedBeaconBlockHeader
  }
  deriving stock (Eq, Show)

data AttesterSlashing = AttesterSlashing
  { att1 :: IndexedAttestation
  , att2 :: IndexedAttestation
  }
  deriving stock (Eq, Show)

data IndexedAttestation = IndexedAttestation
  { attestingIndices :: [Word64]
  , indexedAttestationData :: AttestationData
  , indexedAttestationSignature :: Signature
  }
  deriving stock (Eq, Show)

data Attestation = Attestation
  { aggregationBits :: ByteString
  , attestationData :: AttestationData
  , signature :: Signature
  }
  deriving stock (Eq, Show)

data AttestationData = AttestationData
  { slot :: Word64
  , committeeIndex :: Word64
  , beaconBlockRoot :: Root
  , source :: Checkpoint
  , target :: Checkpoint
  }
  deriving stock (Eq, Show)

data Checkpoint = Checkpoint
  { checkpointEpoch :: Word64
  , checkpointRoot :: Root
  }
  deriving stock (Eq, Show)

data Deposit = Deposit
  { proof :: [Root]
  , depositData :: DepositData
  }
  deriving stock (Eq, Show)

data DepositData = DepositData
  { depositPubKey :: Pubkey
  , depositWithdrawalCredentials :: Root
  , amount :: Word64
  , depositSignature :: Signature
  }
  deriving stock (Eq, Show)

data SignedVoluntaryExit = SignedVoluntaryExit
  { voluntaryExit :: VoluntaryExit
  , voluntaryExitSignature :: Signature
  }
  deriving stock (Eq, Show)

data VoluntaryExit = VoluntaryExit
  { voluntaryExitEpoch :: Word64
  , voluntaryExitValidatorIndex :: Word64
  }
  deriving stock (Eq, Show)

data SyncAggregate = SyncAggregate
  { syncCommitteeBits :: ByteString
  , syncCommitteeSignature :: Signature
  }
  deriving stock (Eq, Show)

data ExecutionPayloadHeader = ExecutionPayloadHeader
  { parentHash :: Root
  , feeRecipient :: Address
  , payloadStateRoot :: Root
  , receiptsRoot :: Root
  , logsBloom :: Bloom
  , prevRandao :: Root
  , blockNumber :: Word64
  , gasLimit :: Word64
  , gasUsed :: Word64
  , timestamp :: Word64
  , extraData :: Root
  , baseFeePerGas :: Root
  , payloadBlockHash :: Root
  , transactionsRoot :: Root
  }
  deriving stock (Eq, Show)

emptyBeaconBlockBodyBellatrixBlinded :: BeaconBlockBodyBellatrixBlinded
emptyBeaconBlockBodyBellatrixBlinded =
  BeaconBlockBodyBellatrixBlinded
    { randaoReveal = [0]
    , eth1Data = emptyEth1Data
    , graffiti = [0]
    , proposerSlashings = []
    , attesterSlashings = []
    , attestations = []
    , deposits = []
    , voluntaryExits = []
    , syncAggregate = emptySyncAggregate
    , executionPayloadHeader = emptyExecutionPayloadHeader
    }

beaconBlockBodyBellatrixBlindedToContainer :: BeaconBlockBodyBellatrixBlinded -> SSZItem
beaconBlockBodyBellatrixBlindedToContainer item =
  let emptyProp = proposerSlashingToContainer emptyProposerSlashing
      emptyAttSlashing = attesterSlashingToContainer emptyAttesterSlashing
      emptyAtt = attestationToContainer emptyAttestation
      emptyDep = depositToContainer emptyDeposit
      emptyVolExit = signedVoluntaryExitToContainer emptySignedVoluntaryExit in
  SContainer
    [ SVector 96 $ map Uint8 (randaoReveal item)
    , eth1DataToContainer (eth1Data item)
    , SVector 32 $ map Uint8 (graffiti item)
    , SList 16 emptyProp $ map proposerSlashingToContainer (proposerSlashings item)
    , SList 2 emptyAttSlashing $ map attesterSlashingToContainer (attesterSlashings item)
    , SList 128 emptyAtt $ map attestationToContainer (attestations item)
    , SList 16 emptyDep $ map depositToContainer (deposits item)
    , SList 16 emptyVolExit $ map signedVoluntaryExitToContainer (voluntaryExits item)
    , syncAggregateToContainer (syncAggregate item)
    , executionPayloadHeaderToContainer (executionPayloadHeader item)
    ]

emptyProposerSlashing :: ProposerSlashing
emptyProposerSlashing =
  ProposerSlashing
    { header1 = emptySignedBeaconBlockHeader
    , header2 = emptySignedBeaconBlockHeader
    }

proposerSlashingToContainer :: ProposerSlashing -> SSZItem
proposerSlashingToContainer item =
  SContainer
    [ signedBeaconBlockHeaderToContainer (header1 item)
    , signedBeaconBlockHeaderToContainer (header2 item)
    ]

emptySignedBeaconBlockHeader :: SignedBeaconBlockHeader
emptySignedBeaconBlockHeader =
  SignedBeaconBlockHeader
    { blockHeader = emptyBeaconBlockHeader
    , headerSignature = [0]
    }

signedBeaconBlockHeaderToContainer :: SignedBeaconBlockHeader -> SSZItem
signedBeaconBlockHeaderToContainer item =
  SContainer
    [ beaconBlockHeaderToContainer (blockHeader item)
    , SVector 96 $ map Uint8 (headerSignature item)
    ]

emptyBeaconBlockHeader :: BeaconBlockHeader
emptyBeaconBlockHeader =
  BeaconBlockHeader
    { headerSlot = 0
    , proposerIndex = 0
    , parentRoot = [0]
    , stateRoot = [0]
    , bodyRoot = [0]
    }

beaconBlockHeaderToContainer :: BeaconBlockHeader -> SSZItem
beaconBlockHeaderToContainer item =
  SContainer
    [ Uint64 $ headerSlot item
    , Uint64 $ proposerIndex item
    , SVector 32 $ map Uint8 (parentRoot item)
    , SVector 32 $ map Uint8 (stateRoot item)
    , SVector 32 $ map Uint8 (bodyRoot item)
    ]

emptyEth1Data :: Eth1Data
emptyEth1Data =
  Eth1Data
    { depositRoot = [0]
    , depositCount = 0
    , blockHash = [0]
    }

eth1DataToContainer :: Eth1Data -> SSZItem
eth1DataToContainer item =
  SContainer
    [ SVector 32 $ map Uint8 (depositRoot item)
    , Uint64 (depositCount item)
    , SVector 32 $ map Uint8 (blockHash item)
    ]

emptyAttesterSlashing :: AttesterSlashing
emptyAttesterSlashing =
  AttesterSlashing
    { att1 = emptyIndexedAttestation
    , att2 = emptyIndexedAttestation
    }

attesterSlashingToContainer :: AttesterSlashing -> SSZItem
attesterSlashingToContainer item =
  let att1' = indexedAttestationToContainer (att1 item)
   in let att2' = indexedAttestationToContainer (att2 item)
       in SContainer
            [ att1'
            , att2'
            ]

emptyIndexedAttestation :: IndexedAttestation
emptyIndexedAttestation =
  IndexedAttestation
    { attestingIndices = []
    , indexedAttestationData = emptyAttestationData
    , indexedAttestationSignature = [0]
    }

indexedAttestationToContainer :: IndexedAttestation -> SSZItem
indexedAttestationToContainer item =
  let sig' = map Uint8 (indexedAttestationSignature item)
      indices' = map Uint64 (attestingIndices item)
      attData' = indexedAttestationData item
      contData = attestationDataToContainer attData' in 
  SContainer
    [ SList 2048 (Uint64 0) indices'
    , contData
    , SVector 96 sig'
    ]

emptyAttestation :: Attestation
emptyAttestation =
  Attestation
    { aggregationBits = B.empty
    , attestationData = emptyAttestationData
    , signature = [0]
    }

attestationToContainer :: Attestation -> SSZItem
attestationToContainer item =
  let sig' = map Uint8 (signature item)
      attData' = attestationData item
      contData = attestationDataToContainer attData' in
  SContainer
    [ SBitlist (aggregationBits item)
    , contData
    , SVector 96 sig'
    ]

emptyAttestationData :: AttestationData
emptyAttestationData =
  AttestationData
    { slot = 0
    , committeeIndex = 0
    , beaconBlockRoot = [0]
    , source = emptyCheckpoint
    , target = emptyCheckpoint
    }

attestationDataToContainer :: AttestationData -> SSZItem
attestationDataToContainer item =
  let root' = map Uint8 (beaconBlockRoot item)
   in SContainer
        [ Uint64 $ slot item
        , Uint64 $ committeeIndex item
        , SVector 32 root'
        , checkpointToContainer (source item)
        , checkpointToContainer (target item)
        ]

emptyCheckpoint :: Checkpoint
emptyCheckpoint = Checkpoint {checkpointEpoch = 0, checkpointRoot = [0]}

checkpointToContainer :: Checkpoint -> SSZItem
checkpointToContainer item =
  let root' = map Uint8 (checkpointRoot item)
   in SContainer [Uint64 $ checkpointEpoch item, SVector 32 root']

emptyDeposit :: Deposit
emptyDeposit =
  Deposit
    { proof = [[0]]
    , depositData = emptyDepositData
    }

depositToContainer :: Deposit -> SSZItem
depositToContainer item =
  let proof' = map (SVector 32 . map Uint8) (proof item)
   in SContainer
        [ SVector 33 proof'
        , depositDataToContainer $ depositData item
        ]

emptyDepositData :: DepositData
emptyDepositData =
  DepositData
    { depositPubKey = [0]
    , depositWithdrawalCredentials = [0]
    , amount = 0
    , depositSignature = [0]
    }

depositDataToContainer :: DepositData -> SSZItem
depositDataToContainer item =
  let pub' = map Uint8 (depositPubKey item)
      withdraw' = map Uint8 (depositWithdrawalCredentials item)
      sig' = map Uint8 (depositSignature item) in
  SContainer
    [ SVector 48 pub'
    , SVector 32 withdraw'
    , Uint64 $ amount item
    , SVector 96 sig'
    ]

signedVoluntaryExitToContainer :: SignedVoluntaryExit -> SSZItem
signedVoluntaryExitToContainer item =
  let sig' = map Uint8 (voluntaryExitSignature item)
   in SContainer
        [ voluntaryExitToContainer $ voluntaryExit item
        , SVector 96 sig'
        ]

emptySignedVoluntaryExit :: SignedVoluntaryExit
emptySignedVoluntaryExit =
  SignedVoluntaryExit
    { voluntaryExit = emptyVoluntaryExit
    , voluntaryExitSignature = [0]
    }

voluntaryExitToContainer :: VoluntaryExit -> SSZItem
voluntaryExitToContainer item =
  SContainer [Uint64 $ voluntaryExitEpoch item, Uint64 $ voluntaryExitValidatorIndex item]

emptyVoluntaryExit :: VoluntaryExit
emptyVoluntaryExit =
  VoluntaryExit
    { voluntaryExitEpoch = 0
    , voluntaryExitValidatorIndex = 0
    }

syncAggregateToContainer :: SyncAggregate -> SSZItem
syncAggregateToContainer item =
  let sig' = map Uint8 (syncCommitteeSignature item)
   in SContainer
        [ SBitvector $ syncCommitteeBits item
        , SVector 96 sig'
        ]

emptySyncAggregate :: SyncAggregate
emptySyncAggregate =
  SyncAggregate
    { syncCommitteeBits = B.replicate 64 0
    , syncCommitteeSignature = [0]
    }

emptyExecutionPayloadHeader :: ExecutionPayloadHeader
emptyExecutionPayloadHeader =
  ExecutionPayloadHeader
    { parentHash = [0]
    , feeRecipient = [0]
    , payloadStateRoot = [0]
    , receiptsRoot = [0]
    , logsBloom = [0]
    , prevRandao = [0]
    , blockNumber = 0
    , gasLimit = 0
    , gasUsed = 0
    , timestamp = 0
    , extraData = []
    , baseFeePerGas = [0]
    , payloadBlockHash = [0]
    , transactionsRoot = [0]
    }

executionPayloadHeaderToContainer :: ExecutionPayloadHeader -> SSZItem
executionPayloadHeaderToContainer item =
  SContainer
    [ SVector 32 (map Uint8 $ parentHash item)
    , SVector 20 (map Uint8 $ feeRecipient item)
    , SVector 32 (map Uint8 $ payloadStateRoot item)
    , SVector 32 (map Uint8 $ receiptsRoot item)
    , SVector 256 (map Uint8 $ logsBloom item)
    , SVector 32 (map Uint8 $ prevRandao item)
    , Uint64 $ blockNumber item
    , Uint64 $ gasLimit item
    , Uint64 $ gasUsed item
    , Uint64 $ timestamp item
    , SList 32 (Uint8 0) (map Uint8 $ extraData item)
    , SVector 32 (map Uint8 $ baseFeePerGas item)
    , SVector 32 (map Uint8 $ payloadBlockHash item)
    , SVector 32 (map Uint8 $ transactionsRoot item)
    ]
