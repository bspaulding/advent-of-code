module DayEight (main) where

import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

main :: String -> IO ()
main input = do
  let notes = map parseLine (lines input)
  print notes
  print digitsByNumSegments
  let uniques = concatMap uniqueDigitsInOutput notes
  putStrLn $ "uniques = " ++ show uniques ++ ", length = " ++ show (length uniques)

  let patterns = map ((\ps -> (ps, buildSegmentMap ps)) . fst) notes
  print $ head patterns

  let decoded = zip (map snd notes) (map decodeDisplay notes)
  putStrLn $ T.unpack $ T.intercalate (T.pack "\n") (map (T.pack . show) decoded)

  let total = sum $ map snd decoded
  putStrLn $ "total = " ++ show total

decodeDisplay :: ([String], [String]) -> Int
decodeDisplay (patterns, outputs) = sum $ map (\(x, i) -> x * 10 ^ i) (zip (reverse digits) [0 ..])
  where
    segmentMap = buildSegmentMap patterns
    digits = map (decodeSegments segmentMap) outputs

sortByLength :: [[a]] -> [[a]]
sortByLength = L.sortBy (\as bs -> if length as > length bs then GT else LT)

buildSegmentMap :: [String] -> Map.Map Char Char
buildSegmentMap patterns = Map.fromList [(a, 'a'), (b, 'b'), (c, 'c'), (d, 'd'), (e, 'e'), (f, 'f'), (g, 'g')]
  where
    [p2, p3, p4, p51, p52, p53, p61, p62, p63, p7] = sortByLength patterns

    -- a is 7 - 1
    a = chomp (== 1) $ freqs (p3 ++ p2)

    -- d is the common segment of 2, 3, 4, 5
    d = chomp (== 4) $ freqs (p4 ++ p51 ++ p52 ++ p53)

    -- e is the uncommon segment of 2,3,4,5
    e = chomp (== 1) $ freqs (p4 ++ p51 ++ p52 ++ p53)

    -- c is the uncommon segment of 0,6,9,d,e
    c = chomp (== 2) $ freqs (p61 ++ p62 ++ p63 ++ [d, e])

    -- f is 1 - c
    f = chomp (== 1) $ freqs (p2 ++ [c])

    -- b is 4 - 1 - d
    b = chomp (== 1) $ freqs (p2 ++ p4 ++ [d])

    -- g is left over
    g = chomp (== 1) $ freqs (p7 ++ [a, b, c, d, e, f])

chomp :: (v -> Bool) -> Map.Map k v -> k
chomp f m = (fst . head . Map.toList) $ Map.filter f m

freqs :: Ord a => [a] -> Map.Map a Int
freqs = foldl getAndInc Map.empty
  where
    getAndInc acc x = Map.insert x (1 + Map.findWithDefault 0 x acc) acc

decodeSegments :: Map.Map Char Char -> String -> Int
decodeSegments segmentMap segments = getDigitBySegments $ L.sort $ map f segments
  where
    f :: Char -> Char
    f k = Map.findWithDefault '?' k segmentMap

uniqueDigitsInOutput :: ([String], [String]) -> [String]
uniqueDigitsInOutput (patterns, outputs) = outputs `L.intersect` allUniqueDigits
  where
    allUniqueDigits = uniqueDigits (patterns ++ outputs)

uniqueDigits :: [String] -> [String]
uniqueDigits = filter isUniqueDigit

isUniqueDigit :: String -> Bool
isUniqueDigit s = 1 == length digits
  where
    numSegments = length s
    digits = Map.findWithDefault [] numSegments digitsByNumSegments

numSegmentsByDigit :: Map.Map Int Int
numSegmentsByDigit = Map.fromList [(0, 6), (1, 2), (2, 5), (3, 5), (4, 4), (5, 5), (6, 6), (7, 3), (8, 7), (9, 6)]

digitsByNumSegments :: Map.Map Int [Int]
digitsByNumSegments = foldl f Map.empty (Map.toList numSegmentsByDigit)
  where
    f :: Map.Map Int [Int] -> (Int, Int) -> Map.Map Int [Int]
    f acc (d, n) =
      Map.alter
        ( \ds -> case ds of
            Nothing -> Just [d]
            Just ds -> Just (d : ds)
        )
        n
        acc

segmentsByDigit :: Map.Map Int String
segmentsByDigit = Map.fromList [(0, "abcefg"), (1, "cf"), (2, "acdeg"), (3, "acdfg"), (4, "bcdf"), (5, "abdfg"), (6, "abdefg"), (7, "acf"), (8, "abcdefg"), (9, "abcdfg")]

getSegmentsForDigit :: Int -> String
getSegmentsForDigit d = Map.findWithDefault "" d segmentsByDigit

digitBySegments :: Map.Map String Int
digitBySegments = Map.fromList $ map flipTuple $ Map.toList segmentsByDigit
  where
    flipTuple (x, y) = (y, x)

getDigitBySegments :: String -> Int
getDigitBySegments k = Map.findWithDefault 0 k digitBySegments

parseLine :: String -> ([String], [String])
parseLine s = (patterns, outputs)
  where
    [pattern, output] = map T.unpack $ T.splitOn (T.pack " | ") (T.pack s)
    patterns = words pattern
    outputs = words output