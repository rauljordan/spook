module Main where

import Data.ByteString qualified as B
import SSZ 
import Text.Printf

main :: IO ()
main = do
  --putStrLn "Serializing"
  --let item = SVector 2 [SList 1 [SUint16 1], SList 2 [SUint16 2, SUint16 3]]
  --let encodedItem = serialize item
  --case encodedItem of
    --Left i -> print i
    --Right i -> print $ toHexString i

  --let another = SContainer [SVector 1 [SBool True], SList 1 [SBool True], SList 1 [SBool True]]
  --let encoded = serialize another
  --putStrLn "Serializing Another..."
  --case encoded of
    --Left a -> print a
    --Right a -> print $ toHexString a

  putStrLn "Serializing Final..."
  let final = SVector 3 [SBool True, SBool False, SBool True]
  let encodedFinal = serialize final
  case encodedFinal of
    Left a -> print a
    Right a -> print $ toHexString a

  print $ deserialize (SVector 3 [SBool False]) (encodedResult encodedFinal) 

toHexString :: ByteString -> String
toHexString = B.foldr' ((<>) . printf "%02x") ""

encodedResult :: SerializationResult a -> ByteString
encodedResult res =
  case res of
    Left _ -> B.empty
    Right el -> el
