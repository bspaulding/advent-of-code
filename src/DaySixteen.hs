module DaySixteen (main) where

import qualified Data.List as List

main :: String -> IO ()
main input = do
  let parsed = map (\line -> (line, parseLine line)) (lines input)
  putStrLn $ List.intercalate "\n\n" $ map (uncurry printParsedPacket) parsed

printParsedPacket :: String -> ([Int], Packet, [Int]) -> String
printParsedPacket line (binary, packet, remaining) =
  List.intercalate "\n" [hex, binary', packet', remaining', sumOfVersions]
  where
    hex = "hex = " ++ line
    binary' = "binary = " ++ concatMap show binary
    packet' = "packet = " ++ show packet
    remaining' = "remaining bits = " ++ concatMap show remaining
    sumOfVersions = "sum of versions = " ++ show (versionSum packet)

parseLine :: String -> ([Int], Packet, [Int])
parseLine l = (binary, packet, remaining)
  where
    binary = expandHex l
    (packet, remaining) = readPacket binary

versionSum :: Packet -> Int
versionSum (Packet v _ (Literal _)) = v
versionSum (Packet v _ (Operator ps)) = v + sum (map versionSum ps)

data Packet = Packet {version :: Int, typeId :: Int, packetData :: PacketData} deriving (Show)

data PacketData = Literal Int | Operator [Packet] deriving (Show)

readPacket :: [Int] -> (Packet, [Int])
readPacket binary = (packet, bitsLeft)
  where
    version = readBinary $ take 3 binary
    typeId = readBinary $ take 3 $ drop 3 binary
    packetDataBits = drop 6 binary
    (packetData, bitsLeft) =
      if typeId == 4
        then readLiteral packetDataBits []
        else readOperator packetDataBits
    packet = Packet {version = version, typeId = typeId, packetData = packetData}

readPackets :: [Int] -> [Packet]
readPackets [] = []
readPackets binary = packet : readPackets remaining
  where
    (packet, remaining) = readPacket binary

readOperator :: [Int] -> (PacketData, [Int])
readOperator [] = (Operator [], [])
readOperator (0 : rest) = (Operator packets, remaining)
  where
    lengthBits = readBinary (take 15 rest)
    remaining = drop (15 + lengthBits) rest
    packetsData = take lengthBits (drop 15 rest)
    packets = readPackets packetsData
readOperator (1 : rest) = (Operator packets, remaining)
  where
    lengthPackets = readBinary (take 11 rest)
    packetsData = drop 11 rest
    (packets, remaining) = foldl (\(acc, bits) i -> applyFst (\p -> acc ++ [p]) (readPacket bits)) ([], packetsData) [0 .. (lengthPackets - 1)]
readOperator (_ : rest) = (Operator [], [])

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