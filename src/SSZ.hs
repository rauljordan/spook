module SSZ where

import Data.Bits
import Data.ByteString qualified as B
import Data.Serialize (
  Serialize,
  encode,
  getWord16le,
  getWord32le,
  getWord64le,
  getWord8,
  runGet,
 )
import Data.Word ()
import Text.Printf

-- Constants.
bytesPerLengthOffset :: Int
bytesPerLengthOffset = 4

bitsPerByte :: Int
bitsPerByte = 8

data SSZItem
  = Uint64 Word64
  | Uint32 Word32
  | Uint16 Word16
  | Uint8 Word8
  | SBool Bool
  | SList Int SSZItem [SSZItem]
  | SVector Int [SSZItem]
  | SContainer [SSZItem]
  | SBitlist B.ByteString -- TODO: Implement a bitlist abstraction.
  | SBitvector B.ByteString -- TODO: Implement a bitvector abstraction.
  deriving stock (Show, Eq)

-- Gets the length of an SSZ item in bytes.
itemLen :: SSZItem -> Int
itemLen (Uint64 _) = 8
itemLen (Uint32 _) = 4
itemLen (Uint16 _) = 2
itemLen (Uint8 _) = 1
itemLen (SBool _) = 1
itemLen (SBitvector enc) = B.length enc
itemLen (SBitlist enc) = B.length enc
itemLen (SVector n (x : _)) =
  let len = itemLen x
   in n * len
itemLen (SList _ kind xs) =
  let len = itemLen kind
   in len * length xs
itemLen (SContainer xs) =
  let lengths = map itemLen xs
   in sum lengths
itemLen _ = 0

-- Gets the default, zero value of an SSZ item type.
zeroVal :: SSZItem -> SSZItem
zeroVal (SList n kind _) = SList n kind []
zeroVal (SVector n (x : _)) = SVector n (replicate n (zeroVal x))
zeroVal (SVector n []) = SVector n []
zeroVal (SBitlist _) = SBitlist B.empty
zeroVal (SBitvector _) = SBitvector B.empty
zeroVal (SContainer (x : xs)) = SContainer (replicate (length xs) (zeroVal x))
zeroVal (SContainer []) = SContainer []
zeroVal (SBool _) = SBool False
zeroVal (Uint64 _) = Uint64 0
zeroVal (Uint32 _) = Uint32 0
zeroVal (Uint16 _) = Uint16 0
zeroVal (Uint8 _) = Uint8 0

-- Checks whether an item is variable size or not.
isVariable :: SSZItem -> Bool
isVariable (SList _ _ _) = True
isVariable (SVector _ _) = False
isVariable (SBitlist _) = True
isVariable (SBitvector _) = False
isVariable (SContainer xs) = any isVariable xs
isVariable _ = False

-- Checks whether an item is fixed size or not.
isFixed :: SSZItem -> Bool
isFixed = not . isVariable

-- Helpers.
offsetToInt :: SSZItem -> Either DeserializationError Int
offsetToInt (Uint32 num) = Right $ fromIntegral num
offsetToInt _ = Left NotAnOffset

-- The cereal package's encoder is big-endian, so we create our
-- own little-endian encoder using a simple composition.
littleEncoder :: (Serialize a) => a -> B.ByteString
littleEncoder = B.reverse . encode

takeNBits :: Int -> ByteString -> [Bool]
takeNBits n bs = take n $ B.foldl' toBool [] bs
  where
    toBool l w = (testBit w <$> [0 .. (finiteBitSize w)]) ++ l

-- Serialization.
type SerializationResult = Either SerializationError B.ByteString
type IntermediateSerializationResult = Either SerializationError [B.ByteString]
data SerializationError
  = BeyondMaxLength Int
  | NoCorrespondingOffset
  deriving stock (Eq, Show)

type FixedPart = Maybe B.ByteString
type Offset = Int
type EncodedOffset = B.ByteString

serialize :: SSZItem -> SerializationResult
serialize (SVector _ xs) = serializeSequence xs
serialize (SList _ _ xs) = serializeSequence xs
serialize (SContainer xs) = serializeSequence xs
serialize (SBitlist xs) = Right xs
serialize (SBitvector xs) = Right xs
serialize (SBool item) = Right $ littleEncoder item
serialize (Uint64 item) = Right $ littleEncoder item
serialize (Uint32 item) = Right $ littleEncoder item
serialize (Uint16 item) = Right $ littleEncoder item
serialize (Uint8 item) = Right $ littleEncoder item

serializeSequence :: [SSZItem] -> SerializationResult
serializeSequence xs = do
  fixedParts <- getFixedParts xs
  variableParts <- getVariableParts xs
  let fixedLengths = getFixedLengths fixedParts
  let variableLengths = getVariableLengths variableParts
  let total = sum $ fixedLengths ++ variableLengths
  if total >= 2 ^ (bytesPerLengthOffset * bitsPerByte)
    then Left $ BeyondMaxLength total
    else do
      let variableOffsets = getVariableOffsets fixedLengths variableLengths
      encodedOffsets <- mapM encodeOffset variableOffsets
      interleaved <- interleaveOffsets fixedParts (reverse encodedOffsets)
      Right $ B.intercalate B.empty (interleaved ++ variableParts)

getFixedParts :: [SSZItem] -> Either SerializationError [FixedPart]
getFixedParts =
  gatherFixedParts []
  where
    gatherFixedParts acc [] = Right acc
    gatherFixedParts acc (x : xs) =
      if isFixed x
        then do
          encoded <- serialize x
          gatherFixedParts (Just encoded : acc) xs
        else gatherFixedParts (Nothing : acc) xs

getVariableParts :: [SSZItem] -> IntermediateSerializationResult
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

getVariableLengths :: [B.ByteString] -> [Int]
getVariableLengths = map B.length

getVariableOffsets :: [Int] -> [Int] -> [Offset]
getVariableOffsets fixedLengths variableLengths =
  let totalFixed = sum fixedLengths
   in let varItems = take (length variableLengths - 1) variableLengths
       in scanl' (+) totalFixed varItems

encodeOffset :: Int -> SerializationResult
encodeOffset offset = do
  encodedOffset <- serialize $ Uint32 (fromIntegral offset)
  Right encodedOffset

-- Builds a list of byte strings where the fixed elements are left as is
-- while the variable ones are replaced by their corresponding offset.
interleaveOffsets :: [FixedPart] -> [EncodedOffset] -> IntermediateSerializationResult
interleaveOffsets =
  interleave []
  where
    interleave acc [] _ = Right acc
    interleave acc (x : xs) [] =
      case x of
        Just fixedItem -> interleave (fixedItem : acc) xs []
        Nothing -> Left NoCorrespondingOffset
    interleave acc (x : xs) (o : os) =
      case x of
        Just fixedItem -> interleave (fixedItem : acc) xs (o : os)
        Nothing -> interleave (o : acc) xs os

-- Deserialization.
type DeserializationResult = Either DeserializationError SSZItem
type IntermediateDeserializationResult = Either DeserializationError [SSZItem]
data DeserializationError
  = EmptyData
  | DecoderLibraryError String
  | NotAnOffset
  | WrongSize Int SSZItem Int String
  | Weird [Int]
  | Other [SSZItem]
  | OtherPure SSZItem
  | BadHex String
  | OtherTwo String String String
  deriving stock (Show)

deserialize :: SSZItem -> B.ByteString -> DeserializationResult
deserialize (SBool _) enc = case takeNBits 1 enc of
  (x : _) -> Right $ SBool x
  [] -> Left EmptyData
deserialize (Uint64 _) enc = do
  case runGet getWord64le enc of
    Left err -> Left $ DecoderLibraryError err
    Right item -> Right $ Uint64 item
deserialize (Uint32 _) enc = do
  case runGet getWord32le enc of
    Left err -> Left $ DecoderLibraryError err
    Right item -> Right $ Uint32 item
deserialize (Uint16 _) enc = do
  case runGet getWord16le enc of
    Left err -> Left $ DecoderLibraryError err
    Right item -> Right $ Uint16 item
deserialize (Uint8 _) enc = do
  case runGet getWord8 enc of
    Left err -> Left $ DecoderLibraryError err
    Right item -> Right $ Uint8 item
deserialize (SBitlist _) enc = do
  Right $ SBitlist enc
deserialize (SBitvector _) enc = do
  Right $ SBitvector enc
deserialize (SVector n (item : _)) enc =
  if isFixed item
    then do
      items <- deserializeFixedSequence item n (B.length enc) enc
      Right $ SVector n items
    else do
      items <- deserializeVariableSequence item enc
      Right $ SVector n items
deserialize (SVector n []) _ = Right $ SVector n []
deserialize (SList n kind _) enc =
  if isFixed kind
    then do
      let numItems = itemLen kind
      items <- deserializeFixedSequence kind numItems (B.length enc) enc
      Right $ SList n kind items
  else do
    items <- deserializeVariableSequence kind enc
    Right $ SList n kind items
deserialize (SContainer xs) enc = do
  fixedItems <- decodeFixedParts xs enc
  offsets <- decodeVariableOffsets xs enc
  offsetInts <- mapM offsetToInt offsets
  case headMaybe offsetInts of
    Nothing -> Right $ SContainer fixedItems
    Just ff -> do
      let variableItems = filter isVariable xs
      let variableEncodedItems = B.drop ff enc
      decodedVariable <- mapM (`deserialize` variableEncodedItems) variableItems
      let finalDecoded = replaceVariableItems fixedItems decodedVariable
      Right $ SContainer finalDecoded

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x : _) = Just x

type FixedItem = SSZItem
type VariableItem = SSZItem

deserializeFixedSequence :: SSZItem -> Int -> Int -> B.ByteString -> IntermediateDeserializationResult
deserializeFixedSequence item numItems encodedLength encoded = do
  let chunkSize = encodedLength `div` numItems
  if chunkSize * numItems == encodedLength
    then do
      let chunks = chunkBytes encoded chunkSize
      decodedChunks <- deserializeChunks item chunks
      Right decodedChunks
  else Left $ WrongSize encodedLength item numItems (toHexString encoded)

deserializeVariableSequence :: SSZItem -> B.ByteString -> IntermediateDeserializationResult
deserializeVariableSequence item encoded = do
  -- Read the first offset to get some useful information about
  -- where the data itself begins.
  firstOffset <- decodeOffset (B.take bytesPerLengthOffset encoded)
  dataStartIndex <- offsetToInt firstOffset
  let encodedItems = B.drop dataStartIndex encoded
  items <- deserialize item encodedItems
  Right [items]

replaceVariableItems :: [FixedItem] -> [VariableItem] -> [SSZItem]
replaceVariableItems fixedItems varItems =
  f fixedItems varItems []
  where
    f [] _ acc =
      reverse acc
    f (x : xs) [] acc =
      f xs [] (x : acc)
    f (x : xs) (y : ys) acc =
      if isFixed x
        then f xs (y : ys) (x : acc)
        else f xs ys (y : acc)

decodeVariableOffsets :: [SSZItem] -> B.ByteString -> IntermediateDeserializationResult
decodeVariableOffsets items =
  f 0 items []
  where
    f _ [] acc _ =
      Right $ reverse acc
    f lastDecodedIndex (x : xs) acc encodedItems =
      if isFixed x
        then
          let l = itemLen x
           in f (lastDecodedIndex + l) xs acc encodedItems
        else do
          let already = B.drop lastDecodedIndex encodedItems
          let sub = B.take bytesPerLengthOffset already
          decoded <- decodeOffset sub
          f (lastDecodedIndex + bytesPerLengthOffset) xs (decoded : acc) encodedItems

decodeFixedParts :: [SSZItem] -> B.ByteString -> IntermediateDeserializationResult
decodeFixedParts items =
  f 0 items []
  where
    f _ [] acc _ =
      Right $ reverse acc
    f lastDecodedIndex (x : xs) acc encodedItems =
      if isFixed x
        then do
          let l = itemLen x
          let already = B.drop lastDecodedIndex encodedItems
          let sub = B.take l already
          decoded <- deserialize x sub
          f (lastDecodedIndex + l) xs (decoded : acc) encodedItems
      else f (lastDecodedIndex + bytesPerLengthOffset) xs (x : acc) encodedItems

toHexString :: ByteString -> String
toHexString = B.foldr' ((<>) . printf "%02x") ""

decodeOffset :: B.ByteString -> DeserializationResult
decodeOffset offset = do
  decoded <- deserialize (Uint32 0) offset
  Right decoded

deserializeChunks :: SSZItem -> [B.ByteString] -> IntermediateDeserializationResult
deserializeChunks item =
  f item []
  where
    f _ acc [] = Right acc
    f i acc (x : xs) = do
      decoded <- deserialize i x
      f i (decoded : acc) xs

chunkBytes :: B.ByteString -> Int -> [B.ByteString]
chunkBytes encoded chunkSize =
  f [] encoded
  where
    f acc e =
      if e == B.empty
        then acc
        else
          let chunk' = B.take chunkSize e
           in f (chunk' : acc) (B.drop chunkSize e)
