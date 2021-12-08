module DayEight (main) where

import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Text as T

main :: String -> IO ()
main input = do
  let notes = map parseLine (lines input)
  print notes
  print digitsByNumSegments
  let uniques = concatMap uniqueDigitsInOutput notes
  putStrLn $ "uniques = " ++ show uniques ++ ", length = " ++ show (length uniques)

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

parseLine :: String -> ([String], [String])
parseLine s = (patterns, outputs)
  where
    [pattern, output] = map T.unpack $ T.splitOn (T.pack " | ") (T.pack s)
    patterns = words pattern
    outputs = words output