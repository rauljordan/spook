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

beaconBlockBodyBellatrixBlindedToContainer :: BeaconBlockBodyBellatrixBlinded -> SSZItem a
beaconBlockBodyBellatrixBlindedToContainer item =
  SContainer
    [ SVector 96 $ map SUint8 (randaoReveal item)
    , eth1DataToContainer (eth1Data item)
    , SVector 32 $ map SUint8 (graffiti item)
    , SList 16 $ map proposerSlashingToContainer (proposerSlashings item)
    , SList 2 $ map attesterSlashingToContainer (attesterSlashings item)
    , SList 128 $ map attestationToContainer (attestations item)
    , SList 16 $ map depositToContainer (deposits item)
    , SList 16 $ map signedVoluntaryExitToContainer (voluntaryExits item)
    , syncAggregateToContainer (syncAggregate item)
    , executionPayloadHeaderToContainer (executionPayloadHeader item)
    ]

emptyProposerSlashing :: ProposerSlashing
emptyProposerSlashing =
  ProposerSlashing
    { header1 = emptySignedBeaconBlockHeader
    , header2 = emptySignedBeaconBlockHeader
    }

proposerSlashingToContainer :: ProposerSlashing -> SSZItem a
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

signedBeaconBlockHeaderToContainer :: SignedBeaconBlockHeader -> SSZItem a
signedBeaconBlockHeaderToContainer item =
  SContainer
    [ beaconBlockHeaderToContainer (blockHeader item)
    , SVector 96 $ map SUint8 (headerSignature item)
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

beaconBlockHeaderToContainer :: BeaconBlockHeader -> SSZItem a
beaconBlockHeaderToContainer item =
  SContainer
    [ SUint64 $ headerSlot item
    , SUint64 $ proposerIndex item
    , SVector 32 $ map SUint8 (parentRoot item)
    , SVector 32 $ map SUint8 (stateRoot item)
    , SVector 32 $ map SUint8 (bodyRoot item)
    ]

emptyEth1Data :: Eth1Data
emptyEth1Data =
  Eth1Data
    { depositRoot = [0]
    , depositCount = 0
    , blockHash = [0]
    }

eth1DataToContainer :: Eth1Data -> SSZItem a
eth1DataToContainer item =
  SContainer
    [ SVector 32 $ map SUint8 (depositRoot item)
    , SUint64 (depositCount item)
    , SVector 32 $ map SUint8 (blockHash item)
    ]

emptyAttesterSlashing :: AttesterSlashing
emptyAttesterSlashing =
  AttesterSlashing
    { att1 = emptyIndexedAttestation
    , att2 = emptyIndexedAttestation
    }

attesterSlashingToContainer :: AttesterSlashing -> SSZItem a
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
    { attestingIndices = [0]
    , indexedAttestationData = emptyAttestationData
    , indexedAttestationSignature = [0]
    }

indexedAttestationToContainer :: IndexedAttestation -> SSZItem a
indexedAttestationToContainer item =
  let sig' = map SUint8 (indexedAttestationSignature item)
   in let indices' = map SUint64 (attestingIndices item)
       in let attData' = indexedAttestationData item
           in let contData = attestationDataToContainer attData'
               in SContainer
                    [ SList 2048 indices'
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

attestationToContainer :: Attestation -> SSZItem a
attestationToContainer item =
  let sig' = map SUint8 (signature item)
   in let attData' = attestationData item
       in let contData = attestationDataToContainer attData'
           in SContainer
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

attestationDataToContainer :: AttestationData -> SSZItem a
attestationDataToContainer item =
  let root' = map SUint8 (beaconBlockRoot item)
   in SContainer
        [ SUint64 $ slot item
        , SUint64 $ committeeIndex item
        , SVector 32 root'
        , checkpointToContainer (source item)
        , checkpointToContainer (target item)
        ]

emptyCheckpoint :: Checkpoint
emptyCheckpoint = Checkpoint {checkpointEpoch = 0, checkpointRoot = [0]}

checkpointToContainer :: Checkpoint -> SSZItem a
checkpointToContainer item =
  let root' = map SUint8 (checkpointRoot item)
   in SContainer [SUint64 $ checkpointEpoch item, SVector 32 root']

emptyDeposit :: Deposit
emptyDeposit =
  Deposit
    { proof = [[0]]
    , depositData = emptyDepositData
    }

depositToContainer :: Deposit -> SSZItem a
depositToContainer item =
  let proof' = map (SVector 32 . map SUint8) (proof item)
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

depositDataToContainer :: DepositData -> SSZItem a
depositDataToContainer item =
  let pub' = map SUint8 (depositPubKey item)
   in let withdraw' = map SUint8 (depositWithdrawalCredentials item)
       in let sig' = map SUint8 (depositSignature item)
           in SContainer
                [ SVector 48 pub'
                , SVector 32 withdraw'
                , SUint64 $ amount item
                , SVector 96 sig'
                ]

signedVoluntaryExitToContainer :: SignedVoluntaryExit -> SSZItem a
signedVoluntaryExitToContainer item =
  let sig' = map SUint8 (voluntaryExitSignature item)
   in SContainer
        [ voluntaryExitToContainer $ voluntaryExit item
        , SVector 96 sig'
        ]

signedEmptyVoluntaryExit :: SignedVoluntaryExit
signedEmptyVoluntaryExit =
  SignedVoluntaryExit
    { voluntaryExit = emptyVoluntaryExit
    , voluntaryExitSignature = [0]
    }

voluntaryExitToContainer :: VoluntaryExit -> SSZItem a
voluntaryExitToContainer item =
  SContainer [SUint64 $ voluntaryExitEpoch item, SUint64 $ voluntaryExitValidatorIndex item]

emptyVoluntaryExit :: VoluntaryExit
emptyVoluntaryExit =
  VoluntaryExit
    { voluntaryExitEpoch = 0
    , voluntaryExitValidatorIndex = 0
    }

syncAggregateToContainer :: SyncAggregate -> SSZItem a
syncAggregateToContainer item =
  let sig' = map SUint8 (syncCommitteeSignature item)
   in SContainer
        [ SBitvector 64 $ syncCommitteeBits item
        , SVector 96 sig'
        ]

emptySyncAggregate :: SyncAggregate
emptySyncAggregate =
  SyncAggregate
    { syncCommitteeBits = B.empty
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

executionPayloadHeaderToContainer :: ExecutionPayloadHeader -> SSZItem a
executionPayloadHeaderToContainer item =
  SContainer
    [ SVector 32 (map SUint8 $ parentHash item)
    , SVector 20 (map SUint8 $ feeRecipient item)
    , SVector 32 (map SUint8 $ payloadStateRoot item)
    , SVector 32 (map SUint8 $ receiptsRoot item)
    , SVector 256 (map SUint8 $ logsBloom item)
    , SVector 32 (map SUint8 $ prevRandao item)
    , SUint64 $ blockNumber item
    , SUint64 $ gasLimit item
    , SUint64 $ gasUsed item
    , SUint64 $ timestamp item
    , SList 32 (map SUint8 $ extraData item)
    , SVector 32 (map SUint8 $ baseFeePerGas item)
    , SVector 32 (map SUint8 $ payloadBlockHash item)
    , SVector 32 (map SUint8 $ transactionsRoot item)
    ]
