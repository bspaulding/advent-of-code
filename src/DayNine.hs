module DayNine (main) where

import qualified Data.Text as Text
import System.Console.ANSI.Codes

main :: String -> IO ()
main input = do
  let heightMap = parseHeightMap input
  putStrLn $ prettyMap heightMap [] ++ "\n"
  let lows = lowPoints heightMap
  putStrLn $ prettyMap heightMap lows ++ "\n"

  let lowValues = map (getMapValue heightMap) lows
  let riskLevel = sum $ map (+ 1) lowValues
  putStrLn $ "risk level = " ++ show riskLevel

  let bs = basins heightMap
  putStrLn $ "basins = " ++ show (basins heightMap)

  putStrLn $ join "\n" $ map (prettyMap heightMap) bs

basins :: [[Int]] -> [[(Int, Int)]]
basins heightMap = []

getMapValue :: [[Int]] -> (Int, Int) -> Int
getMapValue heightMap (y, x) = (heightMap !! y) !! x

lowPoints :: [[Int]] -> [(Int, Int)]
lowPoints heightMap = filter (isLocalMinimum heightMap) (coordsIn heightMap)

coordsIn :: [[Int]] -> [(Int, Int)]
coordsIn heightMap = coords
  where
    height = length heightMap
    width = (length . head) heightMap
    coords = [(y, x) | y <- [0 .. (height - 1)], x <- [0 .. (width - 1)]]

isLocalMinimum :: [[Int]] -> (Int, Int) -> Bool
isLocalMinimum heightMap (y, x) = all (> getMapValue heightMap (y, x)) (neighborsOf heightMap (y, x))

neighborsOf :: [[Int]] -> (Int, Int) -> [Int]
neighborsOf heightMap (y, x) = map (getMapValue heightMap) (neighborCoordsOf heightMap (y, x))

neighborCoordsOf :: [[Int]] -> (Int, Int) -> [(Int, Int)]
neighborCoordsOf heightMap (y, x) = filter (inBounds height width) [(y - 1, x), (y, x - 1), (y + 1, x), (y, x + 1)]
  where
    height = length heightMap
    width = (length . head) heightMap

inBounds :: Int -> Int -> (Int, Int) -> Bool
inBounds height width (y, x) = y >= 0 && x >= 0 && y < height && x < width

prettyMap :: [[Int]] -> [(Int, Int)] -> String
prettyMap m psh = Text.unpack m'
  where
    m' = Text.intercalate (Text.pack "\n") (map Text.pack mapLines)
    mapLines = map (join "") $ chunks ((length . head) m) $ map displayPoint (coordsIn m)

    displayPoint :: (Int, Int) -> String
    displayPoint p = highlight ++ show (getMapValue m p) ++ setSGRCode []
      where
        highlight = if p `elem` psh then setSGRCode [SetColor Foreground Vivid Red] else ""

join :: String -> [String] -> String
join sep xs = Text.unpack $ Text.intercalate (Text.pack sep) (map Text.pack xs)

parseHeightMap :: String -> [[Int]]
parseHeightMap input =
  map (map (\c -> read [c])) $ lines input

chunks :: Int -> [a] -> [[a]]
chunks x xs =
  if length xs <= x
    then [xs]
    else take x xs : chunks x (drop x xs)
