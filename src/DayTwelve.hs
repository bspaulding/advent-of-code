module DayTwelve (main) where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Tuple as Tuple

join :: String -> [String] -> String
join sep xs = Text.unpack $ Text.intercalate (Text.pack sep) (map Text.pack xs)

splitOn :: String -> String -> [String]
splitOn sep s = map Text.unpack $ Text.splitOn (Text.pack sep) (Text.pack s)

type Edge = (String, String)

main :: String -> IO ()
main input = do
  let edges = map parseEdge (lines input)
  putStrLn $ "edges -> \n" ++ join "\n" (map show edges)

  let edges' = edges ++ map Tuple.swap edges
  let paths = pathsFrom edges' ["start"]
  putStrLn $ "paths = \n" ++ join "\n" (List.sort (map (join "," . reverse) paths))
  putStrLn $ "There are " ++ show (length paths) ++ " paths."

pathsFrom :: [Edge] -> [String] -> [[String]]
pathsFrom edges [] = []
pathsFrom edges ("end" : rest) = ["end" : rest]
pathsFrom edges path = concatMap restPaths (candidates path edges)
  where
    restPaths :: Edge -> [[String]]
    restPaths (_, b) = pathsFrom edges (b : path)

candidates :: [String] -> [Edge] -> [Edge]
candidates path = filter candidate
  where
    candidate (a, b) = a == head path && canVisit b path

canVisit :: String -> [String] -> Bool
canVisit b path =
  isUpper b || canRevisit (b : path) || b `notElem` path

canRevisit :: [String] -> Bool
canRevisit path =
  startCount <= 1
    && endCount <= 1
    && allSmallCaveVisitsUnderTwo
    && length smallCavesVisitedTwice < 2
  where
    caveVisits = countElems path

    startCount = Map.findWithDefault 0 "start" caveVisits
    endCount = Map.findWithDefault 0 "end" caveVisits

    smallCaveVisits = Map.filterWithKey (\k _ -> isLower k) caveVisits
    allSmallCaveVisitsUnderTwo = all (<= 2) (Map.elems smallCaveVisits)
    smallCavesVisitedTwice = Map.toList $ Map.filter (== 2) smallCaveVisits

countElems :: Ord a => [a] -> Map.Map a Int
countElems = foldl f Map.empty
  where
    f :: Ord a => Map.Map a Int -> a -> Map.Map a Int
    f acc a = Map.insert a (1 + Map.findWithDefault 0 a acc) acc

isUpper :: String -> Bool
isUpper = all Char.isUpper

isLower :: String -> Bool
isLower = all Char.isLower

parseEdge :: String -> Edge
parseEdge s = (a, b)
  where
    tokens = splitOn "-" s
    a = tokens !! 0
    b = tokens !! 1

edgesTestInput :: String
edgesTestInput = "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end"