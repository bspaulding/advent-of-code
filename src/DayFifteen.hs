module DayFifteen (main) where

import Data.List as List
import qualified Data.Map as Map
import Data.Ord as Ord
import qualified Data.Text as Text
import System.Console.ANSI.Codes

main :: String -> IO ()
main input = do
  let riskMap = map parseLine (lines input)
  let goal = (length riskMap - 1, length (head riskMap) - 1)
  let result = search goal (neighborsAndCostsOf riskMap) (manhattan goal) [((0, 0), 0, 0)] [] (Map.fromList [((0, 0), 0)]) Map.empty
  case result of
    Nothing -> putStrLn "No path found!"
    Just (cost, path) -> do
      putStrLn $ prettyMap riskMap path
      print result
      print $ pathCost riskMap path

neighborsAndCostsOf :: [[Int]] -> Point -> [(Point, Int)]
neighborsAndCostsOf riskMap p = map (\p -> (p, getMapValue riskMap p)) (neighborCoordsOf riskMap p)

neighborCoordsOf :: [[Int]] -> Point -> [Point]
neighborCoordsOf g (y, x) = filter (inBounds height width) [(y - 1, x), (y, x - 1), (y + 1, x), (y, x + 1)]
  where
    height = length g
    width = (length . head) g

inBounds :: Int -> Int -> (Int, Int) -> Bool
inBounds height width (y, x) = y >= 0 && x >= 0 && y < height && x < width

getMapValue :: [[Int]] -> (Int, Int) -> Int
getMapValue map (y, x) = (map !! y) !! x

-- a terrible a star impl, i think
search ::
  (Ord a, Eq a) =>
  a -> -- goal node
  (a -> [(a, Int)]) -> -- returns neighbors with costs
  (a -> Int) -> -- heuristic function, estimate cost toward goal
  [(a, Int, Int)] -> -- open set (node, priority, gcost)
  [a] -> -- visited/closed set
  Map.Map a Int -> -- gscores
  Map.Map a a -> -- backtracking
  Maybe (Int, [a]) -- result, nothing if no path can be found, or just the cost and the path as list of nodes
search goal neighbors h open closed gscores tracks
  -- if open is empty, there's no path
  | null open = Nothing
  -- if node is goal, then we found it!
  | node == goal = Just (gcost, buildPath tracks node)
  -- if node is seen already, discard and move on
  | node `elem` closed = search goal neighbors h open' closed gscores tracks
  -- otherwise expand the search
  | otherwise = search goal neighbors h open'' closed' gscores' tracks'
  where
    (node, priority, gcost) = minimumBy (comparing (\(_, x, _) -> x)) open
    -- remove node from open
    open' = filter ((/= node) . (\(x, _, _) -> x)) open
    -- add node to closed
    closed' = node : closed
    -- get successors with costs
    successors = map (\(a, g) -> (a, gcost + g, h a)) $ neighbors node
    -- filter successors that are not seen
    successorsNotSeen = filter (\(a, gcost, hcost) -> a `notElem` closed' && (not (Map.member a gscores) || Map.findWithDefault (gcost + 1) a gscores > gcost)) successors
    -- insert new successors into open
    open'' = map (\(a, g, h) -> (a, g + h, g)) successorsNotSeen ++ open
    -- insert new successors into scores
    gscores' = foldl (\gs (a, g, _) -> Map.insert a g gs) gscores successorsNotSeen
    -- insert new successors into tracks
    tracks' = foldl (\ts (a, _, _) -> Map.insert a node ts) tracks successorsNotSeen

buildPath :: Ord a => Map.Map a a -> a -> [a]
buildPath tracks node =
  if Map.member node tracks
    then buildPath tracks (Map.findWithDefault node node tracks) ++ [node]
    else [node]

costMap :: [[Int]] -> Map.Map Point Int
costMap riskMap = Map.fromList $ map (\(y, x) -> ((y, x), (riskMap !! y) !! x)) coords
  where
    height = length riskMap - 1
    width = length (head riskMap) - 1
    coords = [(y, x) | x <- [0 .. width], y <- [0 .. height]]

pathCost :: [[Int]] -> [Point] -> Int
pathCost riskMap path =
  sum $ map (\(y, x) -> (riskMap !! y) !! x) (drop 1 path)

manhattan :: Point -> Point -> Int
manhattan (y1, x1) (y2, x2) = d * (dx + dy)
  where
    dx = abs (x1 - x2)
    dy = abs (y1 - y2)
    d = 1

type Point = (Int, Int)

newtype RiskMap = RiskMap [[Int]]

instance Show RiskMap where
  show (RiskMap riskMap) = join "\n" (map (join "" . map show) riskMap)

parseLine :: String -> [Int]
parseLine = map (\c -> read [c])

join :: String -> [String] -> String
join sep xs = Text.unpack $ Text.intercalate (Text.pack sep) (map Text.pack xs)

prettyMap :: [[Int]] -> [(Int, Int)] -> String
prettyMap m psh = Text.unpack m'
  where
    m' = Text.intercalate (Text.pack "\n") (map Text.pack mapLines)
    mapLines = map (join "") $ chunks ((length . head) m) $ map displayPoint (coordsIn m)

    displayPoint :: (Int, Int) -> String
    displayPoint p = highlight ++ show (getMapValue m p) ++ setSGRCode []
      where
        highlight = if p `elem` psh then setSGRCode [SetColor Foreground Vivid Red] else ""

chunks :: Int -> [a] -> [[a]]
chunks x xs =
  if length xs <= x
    then [xs]
    else take x xs : chunks x (drop x xs)

coordsIn :: [[Int]] -> [(Int, Int)]
coordsIn heightMap = coords
  where
    height = length heightMap - 1
    width = (length . head) heightMap - 1
    coords = [(y, x) | y <- [0 .. height], x <- [0 .. width]]