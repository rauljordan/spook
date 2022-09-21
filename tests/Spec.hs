module Main where

import SSZ
import ConsensusTypes

import Data.ByteString qualified as B
import Text.Hex qualified as Hex

import Test.Hspec (describe, hspec, it, shouldBe, shouldNotBe)

main :: IO ()
main = hspec $ do
  describe "Round trip consensus types deserialize/serialize" $ do
    it "attestation" $ do
      let rawData = "e4000000345d4800000000002400000000000000ce089918451bc65139ecb8e80369c2e4be5dff9d1d564654e43f105c91b93330e8420200000000007bae897e657fb6f1a205ba1db98bc8406ae90fbaa70f3408aa251d2c421f08a6e942020000000000a55554f9a0809ad310dce33607118ec4bceef1aafac0ff010673c8a616bd2dea93cda3e8104681a06175ed51996c9a2d4d3b6cd8eca24443b75f2f8cb71bdf05b0f7e5d03ef3c9b9242f29179eb9d29717e8861026b305e49059b0fdedb71e6751224982180e6030fa48def80058695aeef6a98fd111c6e98abf0cd61984d2d0ffffffffffffffffffffffffffffffffbffffffffffffeffffdf0f"
      let shell = attToContainer emptyAttestation
      testRoundTripSSZ shell rawData
    it "execution payload" $ do
      let rawData = "bfd5a460b4d58a569d4e99dc768f68110b75786f089a0a9484b628e8fedd1fa74675c7e5baafbffbca748158becba61ef3b0a26365244b95706e48464450f621771ce848fced8df3be8cf6e02011c058893d0a657b529aae87916b924774a02ef0741e8a59e817de79fcbfb8d79cf7ca89c6af03b22fc3c6558bba8c28d84cd7ae30f7610740444db596832561bd0379bfbdc3beb0af456210d8d312e5dd890e0b7c4b133ab90a988fa9f70410fe28733afb68c8c40314a9259ac5de6b1592dadd459061e34c89a06d4bfc3c3a93904bdcc9b32a5b23702f6af6661135d88bec96244d1936cf9af9602a8f7a8f30449a204874ddf9488f50e3071877c07fb60d256b29bcfc22b445a3a02daeae2b7b7b63389481cb02cbcb86c2e557e993e8ecc5130e73a3d54e802c8bc59b412a8f4e0d4fd7880f8a3c362f28a5ceab455c2064875293c23003f367ff0c35332d69875439a55088b9ad8dd914da873e2b36e79f3938b3d323029b758c6ae9c0fdfd5ff891aa733a9becac0c4fee67e328c9db2ddc1c3215b14ad9477164d179b93c8497588bd7f6b9ed000000000080c3c90100000000102e0e01000000009f9b2a6300000000180200005f7a8f17010000000000000000000000000000000000000000000000000000008d6acdafbad946dbfdbf8c6a547037abd3a352f454efd7c3f3f97fdb2a66e591d7475e9ba509192bc0b664ef4dbd0abae3199f615f2153507a590edc883dae37"
      let shell = executionPayloadHeaderToContainer emptyExecutionPayloadHeader
      testRoundTripSSZ shell rawData

testRoundTripSSZ :: SSZItem a -> Text -> IO ()
testRoundTripSSZ shell rawData = do
    let decodedHex = safeDecodeHex rawData
    let decoded = safeDeserialize shell decodedHex
    decoded `shouldNotBe` shell
    let roundTripEncoded = safeSerialize decoded
    Hex.encodeHex roundTripEncoded `shouldBe` Hex.encodeHex decodedHex

safeDecodeHex :: Text -> B.ByteString
safeDecodeHex item =
  case Hex.decodeHex item of
    Nothing -> error "could not decode hex"
    Just d -> d

safeSerialize :: SSZItem a -> B.ByteString
safeSerialize item =
  case serialize item of
    Left err -> error $ show err
    Right encoded -> encoded

safeDeserialize :: SSZItem a -> B.ByteString -> SSZItem a
safeDeserialize item encoded = 
  case deserialize item encoded of
    Left err -> error $ show err
    Right decoded -> decoded
