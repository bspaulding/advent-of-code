module DayEleven (main) where

import qualified Data.Maybe as Maybe
import Utilities

main :: String -> IO ()
main input = do
  let grid = map parse (lines input)
  putStrLn $ "grid = \n" ++ join "\n" (map show grid)

  let (g10, f10) = runSteps 10 grid
  putStrLn $ "step 10 = \n" ++ join "\n" (map show g10) ++ "\nthere were " ++ show f10 ++ " flashes after 10 steps."

  let (g100, f100) = runSteps 100 grid
  putStrLn $ "step 100 = \n" ++ join "\n" (map show g100) ++ "\nthere were " ++ show f100 ++ " flashes after 100 steps."

runSteps :: Int -> Grid -> (Grid, Int)
runSteps n g = foldl f (g, 0) [0 .. (n - 1)]
  where
    f :: (Grid, Int) -> Int -> (Grid, Int)
    f (g, n) i = (g', n + n')
      where
        g' = step g
        n' = length $ concatMap (filter (== 0)) g'

type Grid = [[Int]]

type Coord = (Int, Int)

incGrid :: Grid -> Grid
incGrid = map (map (+ 1))

step :: Grid -> Grid
step g = flash (coordsIn g) [] g

flash :: [Coord] -> [Coord] -> Grid -> Grid
flash ps psFlashedPrev g = if null ps' then g' else flash ps' psEverFlashed g'
  where
    gcs = gridWithCoords g

    incPs :: (Int, Coord) -> (Int, Coord)
    incPs (e, p) = (e + length (filter (== p) ps), p)
    preflashed = map (map incPs) gcs

    flashed :: [[(Int, Maybe Coord)]]
    flashed = map (map (\(e, p) -> if e > 9 then (0, Just p) else (e, Nothing))) preflashed

    g' :: Grid
    g' = map (map fst) flashed

    psFlashed :: [Coord]
    psFlashed = Maybe.catMaybes (concatMap (map snd) flashed)

    psEverFlashed = psFlashedPrev ++ psFlashed

    ps' :: [Coord]
    ps' = filter (`notElem` psEverFlashed) $ concatMap (neighborCoordsOf g') psFlashed

gridWithCoords :: Grid -> [[(Int, Coord)]]
gridWithCoords g = map (\(es, y) -> zip es (map ((,) y) [0 .. (length es - 1)])) (zip g [0 ..])

getMapValue :: [[Int]] -> (Int, Int) -> Int
getMapValue heightMap (y, x) = (heightMap !! y) !! x

inBounds :: Int -> Int -> (Int, Int) -> Bool
inBounds height width (y, x) = y >= 0 && x >= 0 && y < height && x < width

neighborCoordsOf :: Grid -> Coord -> [Coord]
neighborCoordsOf g (y, x) = filter (inBounds height width) [(y - 1, x), (y, x - 1), (y + 1, x), (y, x + 1), (y - 1, x - 1), (y + 1, x + 1), (y - 1, x + 1), (y + 1, x - 1)]
  where
    height = length g
    width = (length . head) g

coordsIn :: Grid -> [Coord]
coordsIn g = coords
  where
    height = length g
    width = (length . head) g
    coords = [(y, x) | y <- [0 .. (height - 1)], x <- [0 .. (width - 1)]]

parse :: String -> [Int]
parse = map (\c -> read [c])