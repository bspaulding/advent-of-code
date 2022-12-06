module Day6 where

import Data.List (intercalate)
import qualified Data.Map as Map
import System.Environment (getArgs)

main :: IO ()
main = do
  input <- getPuzzleInput
  case input of
    Nothing -> return ()
    Just input -> do
      let inLines = filter (not .null) $ lines input
      let results = map (findStartPacket 0) $ inLines
      putStrLn $ "Day 6\n" <> intercalate "\n" (map show (zip inLines results))

findStartPacket :: Int -> [Char] -> Int
findStartPacket i cs =
  if isStartPacket
     then i + 4
     -- we could run out of bounds forever here if we hit the end
     else findStartPacket (i + 1) (tail cs)
  where
    packet = take 4 cs
    packetCounts = countItems packet
    isStartPacket = all (== 1) (Map.elems packetCounts)

countItems :: Ord x => [x] -> Map.Map x Int
countItems xs = foldl (\acc x -> Map.insertWith (+) x 1 acc) Map.empty xs

getPuzzleInput :: IO (Maybe String)
getPuzzleInput = do
  args <- getArgs
  if length args /= 1 then do
    putStrLn $ "Please provide a filepath as input!"
    return Nothing
  else do
    let inPath = args !! 0
    putStrLn $ "reading input from " <> show (inPath)
    input <- readFile inPath
    putStrLn $ "---- Begin Puzzle Input ----\n" <> input <> "\n---- End Puzzle Input ----"
    return (Just input)
