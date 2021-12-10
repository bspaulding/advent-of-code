module DayNine (main) where

import qualified Data.Text as Text

main :: String -> IO ()
main input = do
  let heightMap = parseHeightMap input
  putStrLn $ prettyMap heightMap
  let lows = lowPoints heightMap
  putStrLn $ "lows = " ++ show lows
  let lowValues = map (getMapValue heightMap) lows
  print (zip lowValues lows)
  let riskLevel = sum $ map (+ 1) lowValues
  putStrLn $ "risk level = " ++ show riskLevel

getMapValue :: [[Int]] -> (Int, Int) -> Int
getMapValue heightMap (y, x) = (heightMap !! y) !! x

lowPoints :: [[Int]] -> [(Int, Int)]
lowPoints heightMap = filter (isLocalMinimum heightMap) coords
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

prettyMap :: [[Int]] -> String
prettyMap m = Text.unpack m'
  where
    m' = Text.intercalate (Text.pack "\n") mapLines
    mapLines = map (Text.intercalate (Text.pack "") . map (Text.pack . show)) m

parseHeightMap :: String -> [[Int]]
parseHeightMap input =
  map (map (\c -> read [c])) $ lines input

chunks :: Int -> [a] -> [[a]]
chunks x xs =
  if length xs <= x
    then [xs]
    else take x xs : chunks x (drop x xs)
