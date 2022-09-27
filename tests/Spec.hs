module Main where

import ConsensusTypes
import SSZ

import Data.ByteString qualified as B
import Text.Hex qualified as Hex

import Test.Hspec (describe, hspec, it, shouldBe, shouldNotBe)

main :: IO ()
main = hspec $ do
  describe "Round trip consensus types deserialize/serialize" $ do
    it "attestation" $ do
      let shell = attestationToContainer emptyAttestation
      runFixtureTest "attestation" shell
    it "execution_payload_header" $ do
      let shell = executionPayloadHeaderToContainer emptyExecutionPayloadHeader
      runFixtureTest "execution_payload_header" shell
    it "eth1_data" $ do
      let shell = eth1DataToContainer emptyEth1Data
      runFixtureTest "eth1_data" shell
    it "sync_aggregate" $ do
      let shell = syncAggregateToContainer emptySyncAggregate
      runFixtureTest "sync_aggregate" shell
    --it "beacon_block_body_blinded_bellatrix" $ do
      --let shell = beaconBlockBodyBellatrixBlindedToContainer emptyBeaconBlockBodyBellatrixBlinded
      --runFixtureTest "beacon_block_body_blinded_bellatrix" shell
    it "list-of-list" $ do
      let item = Hex.decodeHex "040000000400000002"
      case item of 
        Nothing -> do
          error "could not setup test"
        Just i -> do
          testRoundTripSSZ listOfListShell i

listOfList :: SSZItem
listOfList = SList 1 (SContainer [SList 1 (Uint8 0) []]) 
  [
    SContainer [SList 1 (Uint8 0) [Uint8 2]]
  ]

listOfListShell :: SSZItem
listOfListShell = SList 1 (SContainer [SList 1 (Uint8 0) []]) 
  [
    SContainer [SList 1 (Uint8 0) []]
  ]

runFixtureTest :: String -> SSZItem -> IO ()
runFixtureTest testCase shell = do
  fileBytes <- readFileBS $ "tests/fixtures/" <> testCase <> ".ssz"
  testRoundTripSSZ shell fileBytes

testRoundTripSSZ :: SSZItem -> ByteString -> IO ()
testRoundTripSSZ shell rawData = do
  let decoded = safeDeserialize shell rawData
  decoded `shouldNotBe` shell
  let roundTripEncoded = safeSerialize decoded
  Hex.encodeHex roundTripEncoded `shouldBe` Hex.encodeHex rawData

safeSerialize :: SSZItem -> B.ByteString
safeSerialize item =
  case serialize item of
    Left err -> error $ show err
    Right encoded -> encoded

safeDeserialize :: SSZItem -> B.ByteString -> SSZItem
safeDeserialize item encoded =
  case deserialize item encoded of
    Left err -> error $ show err
    Right decoded -> decoded
