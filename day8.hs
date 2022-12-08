module Day8 where

import qualified Data.List as List
import System.Environment (getArgs)

main :: IO ()
main = getPuzzleInput >>= maybe (pure ()) day8

day8 input = do
  putStrLn "Day 8"
  let size = length . head . lines $ input
  putStrLn $ "grid size = " <> show size
  let grid = map parseLine (lines input)
  putStrLn $ "grid = " <> show grid
  let ps = [(x, y) | x <- [0..(size - 1)], y <- [0..(size - 1)]]
  putStrLn $ "ps = " <> show ps
  let visibles = filter (isVisible grid) ps
  putStrLn "Part One:"
  putStrLn $ "visibles = " <> show visibles
  putStrLn $ "num visibles = " <> show (length visibles)
  putStrLn "Part Two:"
  putStrLn $ "max scenic score = " <> show (foldl max 0 (map (scenicScore grid) ps))
  -- putStrLn $ debugPoint grid (2,3)

debugPoint grid p = unlines
  [ "debugging " <> show p
  , "----------"
  , "height = " <> show (get grid p)
  , "scenic score = " <> show (scenicScore grid p)
  , "treesSeenBy = " <> show (map (map (\p -> (p, get grid p))) (treesSeenBy grid p))
  , "score components = " <> show (map length (treesSeenBy grid p))
  ]

scenicScore :: [[Int]] -> (Int, Int) -> Int
scenicScore grid p = product $ map length (treesSeenBy grid p)

treesSeenBy grid p = map (\(ps, xs) -> ps ++ take 1 xs) $ map (span (\p' -> get grid p' < h)) (pathsOut grid p)
  where h = get grid p

isVisible :: [[Int]] -> (Int, Int) -> Bool
isVisible grid p = isOnEdge grid p || any (isTallerThanAll grid p) (pathsOut grid p)

pathsOut :: [[Int]] -> (Int, Int) -> [[(Int, Int)]]
pathsOut grid p@(x, y) = [north, east, south, west]
  where
    north = reverse [(x, y) | y <- [0..y - 1]]
    south = [(x, y) | y <- [y + 1..m]]
    east = reverse [(x,y) | x <- [0..x-1]]
    west = [(x,y) | x <- [x+1..m]]
    m = size - 1
    size = length . head $ grid

isTallerThanAll :: [[Int]] -> (Int, Int) -> [(Int, Int)] -> Bool
isTallerThanAll grid p path = all (> h) hs
  where
    hs = map (get grid) path
    h = get grid p

isOnEdge :: [[Int]] -> (Int, Int) -> Bool
isOnEdge grid (x, y) = x == 0 || y == 0 || x == (size - 1) || y == (size - 1)
  where size = length . head $ grid

inGrid :: [[Int]] -> (Int, Int) -> Bool
inGrid grid p@(x, y) =
  x >= 0 && y >= 0 && x < size && y < size
  where size = length . head $ grid

get :: [[Int]] -> (Int, Int) -> Int
get grid (x, y) = grid !! y !! x

parseLine :: [Char] -> [Int]
parseLine cs = map (\c -> read [c]) cs

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

