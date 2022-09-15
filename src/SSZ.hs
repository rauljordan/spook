module SSZ (
  SSZItem (..),
  SerializationFailure (..),
  zeroVal,
  serialize,
  deserialize,
) where

import Data.Bits qualified as Bits
import Data.ByteString qualified as B
import Data.Serialize (Serialize, encode, getWord16le, getWord32le, getWord64le, getWord8, runGet)
import Data.Word ()

-- Constants.
bytesPerLengthOffset :: Int
bytesPerLengthOffset = 4

bitsPerByte :: Int
bitsPerByte = 8

-- Type class for SSZ items.
data SSZItem a
  = SUint64 Word64
  | SUint32 Word32
  | SUint16 Word16
  | SUint8 Word8
  | SBool Bool
  | SList Int [SSZItem a]
  | SVector Int [SSZItem a]
  | SContainer [SSZItem a]
  | SBitlist Int [Bool]
  | SBitvector Int [Bool]
  deriving stock (Show, Eq)

-- The cereal package's encoder is big-endian, so we create our
-- own little-endian encoder using a simple composition.
littleEncoder :: (Serialize a) => a -> B.ByteString
littleEncoder = B.reverse . encode

-- TODO: Better way to do this???
takeNBits :: Int -> B.ByteString -> [Bool]
takeNBits n bs = take n $ B.foldl' toBool [] bs
  where
    toBool l w = (Bits.testBit w <$> [0 .. (Bits.finiteBitSize w)]) ++ l

-- Deserialization.
deserialize :: SSZItem a -> B.ByteString -> Either String (SSZItem a)
deserialize (SBool _) enc = case takeNBits 1 enc of
  (x : _) -> Right $ SBool x
  [] -> Left "no bytes to deserialize"
deserialize (SUint64 _) enc = do
  let g = runGet getWord64le
  item <- g enc
  Right $ SUint64 item
deserialize (SUint32 _) enc = do
  let g = runGet getWord32le
  item <- g enc
  Right $ SUint32 item
deserialize (SUint16 _) enc = do
  let g = runGet getWord16le
  item <- g enc
  Right $ SUint16 item
deserialize (SUint8 _) enc = do
  let g = runGet getWord8
  item <- g enc
  Right $ SUint8 item
deserialize (SVector n (x : _)) enc =
  if isFixed x
    then case itemLen x of
      Just s -> result
        where
          result = do
            items <- fixedVectorDeserialize x s enc
            return $ SVector n items
      Nothing -> Left "no item length"
    else Left "nothing"
deserialize _ _ = Left "Unsupported"

fixedVectorDeserialize :: SSZItem a -> Int -> B.ByteString -> Either String [SSZItem a]
fixedVectorDeserialize el len xs =
  if B.length xs == 0
    then Right []
    else decodedSlidingWindows
  where
    decodedSlidingWindows = do
      let itemBytes = B.take len xs
      let rest = B.drop len xs
      decodedElem <- deserialize el itemBytes
      continued <- fixedVectorDeserialize el len rest
      return $ decodedElem : continued

-- Serialization.
data SerializationFailure 
  = BeyondMaxLength Int 
  | NoCorrespondingOffset
  | Other [Maybe ByteString]
  deriving stock (Eq, Show)

serialize :: SSZItem a -> Either SerializationFailure B.ByteString
serialize (SBool a) = Right $ littleEncoder a
serialize (SUint64 a) = Right $ littleEncoder a
serialize (SUint32 a) = Right $ littleEncoder a
serialize (SUint16 a) = Right $ littleEncoder a
serialize (SUint8 a) = Right $ littleEncoder a
serialize (SVector _ xs) = serializeSequence xs
serialize (SList _ xs) = serializeSequence xs
serialize (SContainer xs) = serializeSequence xs
serialize _ = Right $ encode False

serializeSequence :: [SSZItem a] -> Either SerializationFailure B.ByteString
serializeSequence xs = do
  fixedParts <- getFixedParts [] xs
  variableParts <- getVariableParts xs
  let fixedLengths = getFixedLengths fixedParts
  let variableLengths = getVariableLengths variableParts
  let total = sum $ fixedLengths++variableLengths
  if total >= 2^(bytesPerLengthOffset*bitsPerByte) 
    then Left $ BeyondMaxLength total
  else do
    let variableOffsets = getVariableOffsets fixedLengths variableLengths
    encodedOffsets <- mapM encodeOffset variableOffsets
    interleaved <- interleaveOffsets fixedParts (reverse encodedOffsets)
    Right $ B.intercalate B.empty (interleaved ++ variableParts)

getFixedParts :: [Maybe ByteString] -> [SSZItem a] -> Either SerializationFailure [Maybe ByteString]
getFixedParts acc [] = Right acc
getFixedParts acc (x:xs) = 
  if isFixed x
    then do
      encoded <- serialize x
      getFixedParts (Just encoded : acc) xs
  else
      getFixedParts (Nothing : acc) xs

getVariableParts :: [SSZItem a] -> Either SerializationFailure [ByteString]
getVariableParts [] = Right []
getVariableParts xs = do
  -- Bubble up any serialization failures using the sequence operator
  -- to turn a list of Either monads into an Either SerializationFailure [ByteString] type.
  let encodedElems = map serialize (filter isVariable xs)
  sequence encodedElems

getFixedLengths :: [Maybe ByteString] -> [Int]
getFixedLengths = map determineFixedLength

determineFixedLength :: Maybe ByteString -> Int
determineFixedLength Nothing = bytesPerLengthOffset
determineFixedLength (Just a) = B.length a

getVariableLengths :: [ByteString] -> [Int]
getVariableLengths = map B.length 

getVariableOffsets :: [Int] -> [Int] -> [Int]
getVariableOffsets fixedLengths variableLengths =
  let totalFixed = sum fixedLengths in
  let varItems = take (length variableLengths - 1) variableLengths in
  scanl' (+) totalFixed varItems

encodeOffset :: Int -> Either SerializationFailure ByteString
encodeOffset offset = do
  encodedOffset <- serialize $ SUint32 (fromIntegral offset)
  Right encodedOffset

-- Builds a list of byte strings where the fixed elements are left as is
-- while the variable ones are replaced by their corresponding offset.
interleaveOffsets :: [Maybe ByteString] -> [ByteString] -> Either SerializationFailure [ByteString]
interleaveOffsets =
  interleave [] where
    interleave acc [] _ = Right acc
    interleave acc (x:xs) [] = 
      case x of
        Just fixedItem -> interleave (fixedItem : acc) xs []
        Nothing -> Left NoCorrespondingOffset
    interleave acc (x:xs) (o:os) =
      case x of
        Just fixedItem -> interleave (fixedItem : acc) xs (o:os)
        Nothing -> interleave (o : acc) xs os

-- Gets the length of an SSZ item in bytes.
itemLen :: SSZItem a -> Maybe Int
itemLen (SBool _) = Just 1
itemLen (SUint64 _) = Just 8
itemLen (SVector n _) = Just n
itemLen (SList _ xs) = Just $ length xs
itemLen _ = Nothing

-- Gets the default, zero value of an SSZ item type.
zeroVal :: (Num b) => SSZItem a -> SSZItem b
zeroVal (SBool _) = SBool False
zeroVal (SList n _) = SList n []
zeroVal (SVector n (x : _)) = SVector n (replicate n (zeroVal x))
zeroVal (SVector n []) = SVector n []
zeroVal (SBitlist n _) = SBitlist n []
zeroVal (SBitvector n _) = SBitvector n (replicate n False)
zeroVal (SContainer (x : xs)) = SContainer (replicate (length xs) (zeroVal x))
zeroVal (SContainer []) = SContainer []
zeroVal (SUint64 _) = SUint64 0
zeroVal (SUint32 _) = SUint32 0
zeroVal (SUint16 _) = SUint16 0
zeroVal (SUint8 _) = SUint8 0

-- Size checks for SSZ items.
isVariable :: SSZItem a -> Bool
isVariable (SList _ _) = True
isVariable (SVector _ _) = False
isVariable (SBitlist _ _) = True
isVariable (SBitvector _ _) = False
isVariable (SContainer xs) = all isVariable xs
isVariable _ = False

isFixed :: SSZItem a -> Bool
isFixed = not . isVariable

-- An SSZ item is zeroed if it equals to the zero value of its type.
-- Because the SSZItem type derives Eq, this is a trivial check.
isZero :: (Eq a, Num a) => SSZItem a -> Bool
isZero item = item == zeroVal item

