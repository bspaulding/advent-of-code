module DaySixteen (main) where

import qualified Data.List as List

main :: String -> IO ()
main input = do
  let parsed = map (\line -> (line, parseLine line)) (lines input)
  putStrLn $ List.intercalate "\n\n" $ map (uncurry printParsedPacket) parsed

printParsedPacket :: String -> ([Int], Packet, [Int]) -> String
printParsedPacket line (binary, packet, remaining) =
  List.intercalate "\n" [hex, binary', packet', remaining', sumOfVersions, value]
  where
    hex = "hex = " ++ line
    binary' = "binary = " ++ concatMap show binary
    packet' = "packet = " ++ show packet
    remaining' = "remaining bits = " ++ concatMap show remaining
    sumOfVersions = "sum of versions = " ++ show (versionSum packet)
    value = "value = " ++ show (evalPacket packet)

parseLine :: String -> ([Int], Packet, [Int])
parseLine l = (binary, packet, remaining)
  where
    binary = expandHex l
    (packet, remaining) = readPacket binary

versionSum :: Packet -> Int
versionSum (Packet v _ (Literal _)) = v
versionSum (Packet v _ (Operator _ ps)) = v + sum (map versionSum ps)

data Packet = Packet {version :: Int, typeId :: Int, packetData :: PacketData} deriving (Show)

data PacketData = Literal Int | Operator Operation [Packet] deriving (Show)

data Operation = Sum | Product | Minimum | Maximum | GreaterThan | LessThan | EqualTo | Unknown deriving (Show)

evalPacket :: Packet -> Int
evalPacket (Packet _ _ (Literal x)) = x
evalPacket (Packet _ _ (Operator Sum ps)) = sum $ map evalPacket ps
evalPacket (Packet _ _ (Operator Product ps)) = product $ map evalPacket ps
evalPacket (Packet _ _ (Operator Minimum ps)) = minimum $ map evalPacket ps
evalPacket (Packet _ _ (Operator Maximum ps)) = maximum $ map evalPacket ps
evalPacket (Packet _ _ (Operator GreaterThan ps)) = if evalPacket (head ps) > evalPacket (ps !! 1) then 1 else 0
evalPacket (Packet _ _ (Operator LessThan ps)) = if evalPacket (head ps) < evalPacket (ps !! 1) then 1 else 0
evalPacket (Packet _ _ (Operator EqualTo ps)) = if evalPacket (head ps) == evalPacket (ps !! 1) then 1 else 0
evalPacket (Packet _ _ (Operator Unknown ps)) = 0

readPacket :: [Int] -> (Packet, [Int])
readPacket binary = (packet, bitsLeft)
  where
    version = readBinary $ take 3 binary
    typeId = readBinary $ take 3 $ drop 3 binary
    packetDataBits = drop 6 binary
    (packetData, bitsLeft) =
      if typeId == 4
        then readLiteral packetDataBits []
        else applyFst (Operator op) $ readOperator packetDataBits
    packet = Packet {version = version, typeId = typeId, packetData = packetData}
    op = operation typeId

readPackets :: [Int] -> [Packet]
readPackets [] = []
readPackets binary = packet : readPackets remaining
  where
    (packet, remaining) = readPacket binary

operation :: Int -> Operation
operation 0 = Sum
operation 1 = Product
operation 2 = Minimum
operation 3 = Maximum
operation 5 = GreaterThan
operation 6 = LessThan
operation 7 = EqualTo
operation _ = Unknown

readOperator :: [Int] -> ([Packet], [Int])
readOperator [] = ([], [])
readOperator (0 : rest) = (packets, remaining)
  where
    lengthBits = readBinary (take 15 rest)
    remaining = drop (15 + lengthBits) rest
    packetsData = take lengthBits (drop 15 rest)
    packets = readPackets packetsData
readOperator (1 : rest) = (packets, remaining)
  where
    lengthPackets = readBinary (take 11 rest)
    packetsData = drop 11 rest
    (packets, remaining) = foldl (\(acc, bits) i -> applyFst (\p -> acc ++ [p]) (readPacket bits)) ([], packetsData) [0 .. (lengthPackets - 1)]
readOperator (_ : rest) = ([], [])

applyFst :: (a -> c) -> (a, b) -> (c, b)
applyFst f (a, b) = (f a, b)

readLiteral :: [Int] -> [Int] -> (PacketData, [Int])
readLiteral bits acc =
  if head chunk == 0
    then (Literal $ readBinary (acc ++ tail chunk), remaining)
    else readLiteral (drop 5 bits) (acc ++ tail chunk)
  where
    chunk = take 5 bits
    remaining = drop 5 bits

expandHex :: String -> [Int]
expandHex = concatMap expandHexDigit

expandHexDigit :: Char -> [Int]
expandHexDigit '0' = [0, 0, 0, 0]
expandHexDigit '1' = [0, 0, 0, 1]
expandHexDigit '2' = [0, 0, 1, 0]
expandHexDigit '3' = [0, 0, 1, 1]
expandHexDigit '4' = [0, 1, 0, 0]
expandHexDigit '5' = [0, 1, 0, 1]
expandHexDigit '6' = [0, 1, 1, 0]
expandHexDigit '7' = [0, 1, 1, 1]
expandHexDigit '8' = [1, 0, 0, 0]
expandHexDigit '9' = [1, 0, 0, 1]
expandHexDigit 'A' = [1, 0, 1, 0]
expandHexDigit 'B' = [1, 0, 1, 1]
expandHexDigit 'C' = [1, 1, 0, 0]
expandHexDigit 'D' = [1, 1, 0, 1]
expandHexDigit 'E' = [1, 1, 1, 0]
expandHexDigit 'F' = [1, 1, 1, 1]
expandHexDigit _ = []

readBinary :: [Int] -> Int
readBinary xs = sum $ zipWith (\b e -> (2 ^ e) * b) (reverse xs) [0 ..]