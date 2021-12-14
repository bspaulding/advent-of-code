module DayTwelve (main) where

import qualified Data.Char as Char
import qualified Data.List as List
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
candidates [] _ = []
candidates (label : path) edges = filter candidate edges
  where
    candidate (a, b) = a == label && (isRevisitable b || b `notElem` path)

isRevisitable :: String -> Bool
isRevisitable cs = map Char.toUpper cs == cs

parseEdge :: String -> Edge
parseEdge s = (a, b)
  where
    tokens = splitOn "-" s
    a = tokens !! 0
    b = tokens !! 1