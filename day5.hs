module Day5 where

import Data.Char (isAlpha)
import Data.Foldable (toList)
import qualified Data.Map as Map
import Data.Sequence (fromList, mapWithIndex)
import System.Environment (getArgs)

main :: IO ()
main = do
  maybeInput <- getPuzzleInput
  case maybeInput of
    Nothing -> return ()
    Just input ->  do
      let (cols, movesStrs) = parseColumns (Map.empty, lines input)
      print cols
      let moves = parseMoves movesStrs
      -- print moves
      -- show all the moves, for debug
      -- let movedCols = scanl evalMove cols moves
      -- putStrLn $ unlines $ map showColumns movedCols
      let final = foldl evalMove cols moves
      putStrLn $ "final position = \n" <> showColumns final
      print $ map (headOr ' ' . snd) $ Map.toAscList final
      return ()

headOr :: x -> [x] -> x
headOr d [] = d
headOr _ (x:xs) = x

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

type Columns = Map.Map Int [Char]

showColumns :: Columns -> String
showColumns cols =
  unlines $ map (\(b, cs) -> show b <> " " <> reverse cs)  $ Map.toAscList cols

parseColumns :: (Columns, [String]) -> (Columns, [String])
parseColumns (cols, (l:ls)) =
  if length l == 0 then
    (cols, ls)
  else
    parseColumns (parseColLine cols l, ls)
  where
    parseColLine :: Columns -> String -> Columns
    parseColLine cols l =
      foldl insertCrate cols
      $ filter (isAlpha . snd)
      $ toList
      $ mapWithIndex (\i c -> (lineColToBucket i, c)) (fromList l)

    insertCrate :: Columns -> (Int, Char) -> Columns
    insertCrate cols (b,c) = Map.insertWith (flip (++)) b [c] cols

    lineColToBucket i = (i + 3) `div` 4

data Move = Move { n :: Int, from :: Int, to :: Int } deriving (Show)

parseMoves :: [String] -> [Move]
parseMoves = map parseMove

parseMove :: String -> Move
parseMove xs = Move { n = read (ws !! 1), from = read (ws !! 3), to = read (ws !! 5) }
   where
     ws = words xs

evalMove :: Columns -> Move -> Columns
evalMove cols (Move { n, from, to }) =
  Map.adjust (moved ++) to $
  Map.adjust (drop n) from cols
  where
    moved = reverse $ take n $ Map.findWithDefault [] from cols
