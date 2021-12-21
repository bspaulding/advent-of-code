module DayFifteen (main) where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.Set as Set
import qualified Data.Text as Text
import System.Console.ANSI.Codes

main :: String -> IO ()
main input = do
  let smallMap = map parseLine (lines input)
  let riskMap = genBigMap smallMap 5
  let goal = (length riskMap - 1, length (head riskMap) - 1)
  let open = PQ.singleton 0 ((0, 0), 0)
  let neighborCostsMap = neighborsAndCostsMap riskMap (costMap riskMap)
  let hMap = foldl (\acc p -> Map.insert p (manhattan goal p) acc) Map.empty (coordsIn riskMap)
  let result = search goal (flip (Map.findWithDefault []) neighborCostsMap) (flip (Map.findWithDefault 0) hMap) open Set.empty (Map.fromList [((0, 0), 0)]) Map.empty
  case result of
    Nothing -> putStrLn "No path found!"
    Just (cost, path) -> do
      -- putStrLn $ prettyMap riskMap path
      print cost

genBigMap :: [[Int]] -> Int -> [[Int]]
genBigMap g n = chunks w' $ map bigMapValue coords
  where
    h = length g
    w = length (head g)
    h' = h * n
    w' = w * n

    coords = [(y, x) | y <- [0 .. (h' - 1)], x <- [0 .. (w' - 1)]]

    bigMapValue :: Point -> Int
    bigMapValue (y, x) = if n > 9 then n `mod` 9 else n
      where
        n = (x `div` w) + (y `div` h) + getMapValue g (y `mod` h, x `mod` w)

neighborsAndCostsMap :: [[Int]] -> CostMap -> Map.Map Point [(Point, Int)]
neighborsAndCostsMap riskMap costMap =
  foldl (\acc p -> Map.insert p (neighborsAndCostsOf riskMap costMap p) acc) Map.empty (coordsIn riskMap)

neighborsAndCostsOf :: [[Int]] -> CostMap -> Point -> [(Point, Int)]
neighborsAndCostsOf riskMap costMap p = map (\p -> (p, Map.findWithDefault 0 p costMap)) (neighborCoordsOf height width p)
  where
    height = length riskMap
    width = (length . head) riskMap

neighborCoordsOf :: Int -> Int -> Point -> [Point]
neighborCoordsOf height width (y, x) = filter (inBounds height width) [(y - 1, x), (y, x - 1), (y + 1, x), (y, x + 1)]

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
  PQ.MinPQueue Int (a, Int) -> -- open set: priority (node, gcost)
  Set.Set a -> -- visited/closed set
  Map.Map a Int -> -- gscores
  Map.Map a a -> -- backtracking
  Maybe (Int, [a]) -- result, nothing if no path can be found, or just the cost and the path as list of nodes
search goal neighbors h open closed gscores tracks
  -- if open is empty, there's no path
  | null open = Nothing
  -- if node is goal, then we found it!
  | node == goal = Just (gcost, buildPath tracks node)
  -- if node is seen already, discard and move on
  | node `Set.member` closed = search goal neighbors h open' closed gscores tracks
  -- otherwise expand the search
  | otherwise = search goal neighbors h open'' closed' gscores' tracks'
  where
    -- find and remove minimum node from open
    ((priority, (node, gcost)), open') = PQ.deleteFindMin open
    -- add node to closed
    closed' = Set.insert node closed
    -- get successors with costs
    successors = map (\(a, g) -> (a, gcost + g, h a)) $ neighbors node
    -- filter successors that are not seen
    successorsNotSeen =
      filter
        ( \(a, gcost, hcost) ->
            a `Set.notMember` closed'
              && ( not (Map.member a gscores)
                     || Map.findWithDefault gcost a gscores > gcost
                 )
        )
        successors
    -- insert new successors into open
    open'' = List.foldl' folder open' successorsNotSeen
    folder :: PQ.MinPQueue Int (a, Int) -> (a, Int, Int) -> PQ.MinPQueue Int (a, Int)
    folder acc (a, g, h) = PQ.insert (g + h) (a, g) acc
    -- insert new successors into scores
    gscores' = List.foldl' (\gs (a, g, _) -> Map.insert a g gs) gscores successorsNotSeen
    -- insert new successors into tracks
    tracks' = List.foldl' (\ts (a, _, _) -> Map.insert a node ts) tracks successorsNotSeen

buildPath :: Ord a => Map.Map a a -> a -> [a]
buildPath tracks node =
  if Map.member node tracks
    then buildPath tracks (Map.findWithDefault node node tracks) ++ [node]
    else [node]

type CostMap = Map.Map Point Int

costMap :: [[Int]] -> CostMap
costMap riskMap = Map.fromList $ map (\(y, x) -> ((y, x), (riskMap !! y) !! x)) coords
  where
    height = length riskMap - 1
    width = length (head riskMap) - 1
    coords = [(y, x) | x <- [0 .. width], y <- [0 .. height]]

manhattan :: Point -> Point -> Int
manhattan (y1, x1) (y2, x2) = dx + dy
  where
    dx = abs (x1 - x2)
    dy = abs (y1 - y2)

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