module Main where

import Data.ByteString qualified as B
import SSZ 
import Text.Printf

main :: IO ()
main = do
  putStrLn ""
  putStrLn "Initial value"
  let final = SContainer [SVector 1 [SUint16 2], SList 2 [SUint16 3], SVector 2 [SUint16 4, SUint16 5]]
  print final
  putStrLn "Serializing..."
  let encodedFinal = serialize final
  case encodedFinal of
    Left a -> print a
    Right a -> print $ toHexString a

  putStrLn "Deserializing..."
  let shell = SContainer [SVector 1 [SUint16 0], SList 2 [SUint16 0], SVector 2 [SUint16 0, SUint16 0]]
  print $ deserialize shell (encodedResult encodedFinal) 

toHexString :: ByteString -> String
toHexString = B.foldr' ((<>) . printf "%02x") ""

encodedResult :: SerializationResult a -> ByteString
encodedResult res =
  case res of
    Left _ -> undefined
    Right el -> el
