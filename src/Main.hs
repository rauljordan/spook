module Main where

import Data.ByteString qualified as B
import SSZ 
import Text.Hex qualified as Hex
import Text.Printf
import Data.Text qualified as T

buildCheckpoint str = 
  case Hex.decodeHex str of
    Nothing -> Right $ SUint16 0
    Just encoded -> do
      let checkpoint = SContainer [SUint64 0, SVector 32 [SUint8 0]]
      decoded <- deserialize checkpoint encoded
      Right decoded

main :: IO ()
main = do
  let item = T.pack "ae33000000000000e1523f4aa1d685ff11ab2953b1295a894a8d6d052034df785390b5772aa70a1b"
  putStrLn ""
  putStrLn ""
  putStrLn "Loading checkpoint..."
  print item
  putStrLn ""
  case buildCheckpoint item of
    Left a -> do
      print a
    Right a -> do
      putStrLn "Got checkpoint"
      print a
      putStrLn ""
      case serialize a of
        Left b -> print b
        Right b -> do
          putStrLn "Serialing again"
          print $ toHexString b

toHexString :: ByteString -> String
toHexString = B.foldr' ((<>) . printf "%02x") ""

encodedResult :: SerializationResult a -> ByteString
encodedResult res =
  case res of
    Left _ -> undefined
    Right el -> el
