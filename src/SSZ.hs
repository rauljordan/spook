module SSZ (
  SSZItem (..),
  SerializationResult,
  DeserializationResult,
  zeroVal,
  serialize,
  deserialize,
) where

import Data.Bits qualified as Bits
import Data.ByteString qualified as B
import Data.Serialize (Serialize, encode, getWord16le, getWord32le, getWord64le, getWord8, runGet)
import Data.Word ()
import Text.Printf

-- Constants.
bytesPerLengthOffset :: Int
bytesPerLengthOffset = 4

bitsPerByte :: Int
bitsPerByte = 8

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

-- Helpers.
offsetToInt :: SSZItem a -> Either (DeserializationError a) Int
offsetToInt (SUint32 num) = Right $ fromIntegral num
offsetToInt _ = Left $ UnknownError "not int"

-- The cereal package's encoder is big-endian, so we create our
-- own little-endian encoder using a simple composition.
littleEncoder :: (Serialize a) => a -> ByteString
littleEncoder = B.reverse . encode

takeNBits :: Int -> ByteString -> [Bool]
takeNBits n bs = take n $ B.foldl' toBool [] bs
  where
    toBool l w = (Bits.testBit w <$> [0 .. (Bits.finiteBitSize w)]) ++ l

-- Deserialization.
type DeserializationResult a = Either (DeserializationError a) (SSZItem a) 
type IntermediateDeserializationResult a = Either (DeserializationError a) [SSZItem a]
data DeserializationError a
  = EmptyData 
  | UnknownError String 
  | WrongSize Int
  | WrongOff (SSZItem a)
  | WrongItems [SSZItem a]
  | Mai [[SSZItem a]]
  | BadOffsets [Int]
  | BadHex String
  deriving stock Show

deserialize :: SSZItem a -> ByteString -> DeserializationResult a
deserialize (SBool _) enc = case takeNBits 1 enc of
  (x : _) -> Right $ SBool x
  [] -> Left EmptyData
deserialize (SUint64 _) enc = do
  case runGet getWord64le enc of
    Left err -> Left $ UnknownError err
    Right item -> Right $ SUint64 item
deserialize (SUint32 _) enc = do
  case runGet getWord32le enc of
    Left err -> Left $ UnknownError err
    Right item -> Right $ SUint32 item
deserialize (SUint16 _) enc = do
  case runGet getWord16le enc of
    Left err -> Left $ UnknownError err
    Right item -> Right $ SUint16 item
deserialize (SUint8 _) enc = do
  case runGet getWord8 enc of
    Left err -> Left $ UnknownError err
    Right item -> Right $ SUint8 item
deserialize (SVector n (item : _)) enc =
  if isFixed item
    then do
      items <- deserializeFixedSequence item n (B.length enc) enc
      Right $ SVector n items
  else do
      items <- deserializeVariableSequence item enc
      Right $ SVector n items
deserialize (SList n (item : xs)) enc =
  if isFixed item
    then do
      let numItems = length (item : xs)
      items <- deserializeFixedSequence item numItems (B.length enc) enc
      Right $ SList n items
  else do
      items <- deserializeVariableSequence item enc
      Right $ SList n items
deserialize (SContainer xs) enc = do
  fixedItems <- decodeFixedParts 0 xs [] enc
  offsets<- decodeVariableOffsets 0 xs [] enc
  offsetInts <- mapM offsetToInt offsets
  case firstOffset offsetInts of
    Nothing -> Right $ SContainer fixedItems
    Just ff -> do
      let variableItems = filter isVariable xs
      let variableEncodedItems = B.drop ff enc
      let itemLengths = diffs offsetInts
      decodedVariable <- mapM (\x -> decodeWithItemLengths x [] itemLengths variableEncodedItems) variableItems
      let decodedVariableItems = concat decodedVariable
      let finalDecoded = replaceVariableItems fixedItems decodedVariableItems []
      Right $ SContainer finalDecoded
deserialize _ _ = Left $ UnknownError "Unimplemented"

firstOffset :: [Int] -> Maybe Int
firstOffset [] = Nothing
firstOffset (x:_) = Just x

replaceVariableItems :: [SSZItem a] -> [SSZItem a] -> [SSZItem a] -> [SSZItem a]
replaceVariableItems [] _ acc =
  reverse acc
replaceVariableItems (x:xs) [] acc =
  replaceVariableItems xs [] (x:acc)
replaceVariableItems (x:xs) (y:ys) acc =
  if isFixed x
    then
      replaceVariableItems xs (y:ys) (x:acc)
  else
    replaceVariableItems xs ys (y:acc)

decodeVariableOffsets _ [] acc _ =
  Right $ reverse acc
decodeVariableOffsets lastDecodedIndex (x:xs) acc encoded =
  if isFixed x
    then
      let l = itemLen x in
      decodeVariableOffsets (lastDecodedIndex+l) xs acc encoded
  else do
    let already = B.drop lastDecodedIndex encoded
    let sub = B.take bytesPerLengthOffset already
    decoded <- decodeOffset sub
    decodeVariableOffsets (lastDecodedIndex+bytesPerLengthOffset) xs (decoded:acc) encoded

decodeFixedParts _ [] acc _ =
  Right $ reverse acc
decodeFixedParts lastDecodedIndex (x:xs) acc encoded =
  if isFixed x
    then do
      let l = itemLen x
      let already = B.drop lastDecodedIndex encoded
      let sub = B.take l already
      decoded <- deserialize x sub
      decodeFixedParts (lastDecodedIndex+l) xs (decoded:acc) encoded
  else
     decodeFixedParts (lastDecodedIndex+4) xs (x:acc) encoded

deserializeFixedSequence :: SSZItem a -> Int -> Int -> ByteString -> IntermediateDeserializationResult a
deserializeFixedSequence item numItems encodedLength encoded = do
  let chunkSize = encodedLength `div` numItems
  if chunkSize * numItems == encodedLength
    then do
      let chunks = chunkBytes encoded chunkSize
      decodedChunks <- deserializeChunks item chunks
      Right decodedChunks
  else
    Left $ WrongSize 0 -- TODO: Fix up

deserializeVariableSequence :: SSZItem a -> ByteString -> IntermediateDeserializationResult a
deserializeVariableSequence item encoded = do
  -- Read the first offset to get some useful information about
  -- where the data itself begins.
  firstOffset <- decodeOffset (B.take bytesPerLengthOffset encoded)
  dataStartIndex <- offsetToInt firstOffset
  let offsetsBytes = chunkBytes (B.take dataStartIndex encoded) bytesPerLengthOffset
  decodedOffsets <- mapM decodeOffset offsetsBytes
  offsets <- mapM offsetToInt decodedOffsets
  let itemLengths = map abs (diffs offsets)
  -- From here, determine the lengths of the elements...
  let encodedItems = B.drop dataStartIndex encoded
  items <- decodeWithItemLengths item [] itemLengths encodedItems
  Right $ reverse items

diffs :: [Int] -> [Int]
diffs [] = []
diffs [x] = [x `div` bytesPerLengthOffset]
diffs (x:xs) = zipWith (-) xs (x:xs)

decodeWithItemLengths :: SSZItem a -> [SSZItem a] -> [Int] -> ByteString -> Either (DeserializationError a) [SSZItem a]
decodeWithItemLengths item acc (x:xs) encoded =
  if B.length encoded == 0
    then
      Right acc
  else do
    decoded <- deserialize item (B.take x encoded)
    decodeWithItemLengths item (decoded : acc) xs (B.drop x encoded)
decodeWithItemLengths item acc [] encoded = do
  if B.length encoded == 0
    then
      Right acc
  else do
    let len = B.length encoded
    decoded <- deserialize item (B.take len encoded)
    decodeWithItemLengths item (decoded : acc) [] (B.drop len encoded)

toHexString :: ByteString -> String
toHexString = B.foldr' ((<>) . printf "%02x") ""
    --
decodeOffset :: ByteString -> DeserializationResult a
decodeOffset offset = do
  decoded <- deserialize (SUint32 0) offset
  Right decoded

deserializeChunks :: SSZItem a -> [ByteString] -> IntermediateDeserializationResult a
deserializeChunks item =
  f item [] where
    f _ acc [] = Right acc
    f i acc (x:xs) = do
      decoded<-deserialize i x
      f i (decoded:acc) xs

chunkBytes :: ByteString -> Int -> [ByteString]
chunkBytes encoded chunkSize =
  f [] encoded where
    f acc e =
      if e == B.empty
        then 
          acc
      else
        let chunk' = B.take chunkSize e in
        f (chunk':acc) (B.drop chunkSize e)

-- Serialization.
type SerializationResult a = Either SerializationError ByteString
type IntermediateSerializationResult a = Either SerializationError [ByteString]
data SerializationError 
  = BeyondMaxLength Int 
  | NoCorrespondingOffset
  | Other [Maybe ByteString]
  deriving stock (Eq, Show)

type FixedPart = Maybe ByteString
type Offset = Int
type EncodedOffset = ByteString

serialize :: SSZItem a -> SerializationResult a
serialize (SBool a) = Right $ littleEncoder a
serialize (SUint64 a) = Right $ littleEncoder a
serialize (SUint32 a) = Right $ littleEncoder a
serialize (SUint16 a) = Right $ littleEncoder a
serialize (SUint8 a) = Right $ littleEncoder a
serialize (SVector _ xs) = serializeSequence xs
serialize (SList _ xs) = serializeSequence xs
serialize (SContainer xs) = serializeSequence xs
serialize _ = Right $ encode False

serializeSequence :: [SSZItem a] -> SerializationResult a
serializeSequence xs = do
  fixedParts <- getFixedParts xs
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

getFixedParts :: [SSZItem a] -> Either SerializationError [FixedPart]
getFixedParts =
  gatherFixedParts [] where
    gatherFixedParts acc [] = Right acc
    gatherFixedParts acc (x:xs) = 
      if isFixed x
        then do
          encoded <- serialize x
          gatherFixedParts (Just encoded : acc) xs
      else
          gatherFixedParts (Nothing : acc) xs

getVariableParts :: [SSZItem a] -> IntermediateSerializationResult a
getVariableParts [] = Right []
getVariableParts xs = do
  -- Bubble up any serialization failures using the sequence operator
  -- to turn a list of SerializationResults into an IntermediateSerializationResult
  let encodedElems = map serialize (filter isVariable xs)
  sequence encodedElems

getFixedLengths :: [FixedPart] -> [Int]
getFixedLengths = map determineFixedLength

determineFixedLength :: FixedPart -> Int
determineFixedLength Nothing = bytesPerLengthOffset
determineFixedLength (Just a) = B.length a

getVariableLengths :: [ByteString] -> [Int]
getVariableLengths = map B.length 

getVariableOffsets :: [Int] -> [Int] -> [Offset]
getVariableOffsets fixedLengths variableLengths =
  let totalFixed = sum fixedLengths in
  let varItems = take (length variableLengths - 1) variableLengths in
  scanl' (+) totalFixed varItems

encodeOffset :: Int -> SerializationResult a
encodeOffset offset = do
  encodedOffset <- serialize $ SUint32 (fromIntegral offset)
  Right encodedOffset

-- Builds a list of byte strings where the fixed elements are left as is
-- while the variable ones are replaced by their corresponding offset.
interleaveOffsets :: [FixedPart] -> [EncodedOffset] -> IntermediateSerializationResult a
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
itemLen :: SSZItem a -> Int
itemLen (SBool _) = 1
itemLen (SUint64 _) = 8
itemLen (SUint32 _) = 4
itemLen (SUint16 _) = 2
itemLen (SUint8 _) = 1
itemLen (SVector n (x:_)) =
  let len = itemLen x in
  n * len
itemLen (SList _ (x:xs)) =
  let len = itemLen x in
  len * length (x:xs)
itemLen (SContainer xs) =
  let lengths = map itemLen xs in
  sum lengths
itemLen _ = 0

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

