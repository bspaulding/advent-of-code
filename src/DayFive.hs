module DayFive (main) where

import qualified Data.Map.Strict as Map
import Data.List (intercalate)
import qualified Data.Text as T

data Point = Point Int Int deriving (Eq, Ord, Show)

x :: Point -> Int
x (Point x _) = x

y :: Point -> Int
y (Point _ y) = y

data Line = Line Point Point deriving (Show)

main :: String -> IO ()
main input = do
    let ventLines = map parseVent $ lines input
    putStrLn $ "vent lines parsed = " ++ show ventLines
    let nonDiagonals = filter (\l -> isHorizontal l || isVertical l) ventLines
    putStrLn $ "non-diagonals = " ++ show nonDiagonals
    let ventMap = buildVentMap nonDiagonals
    putStrLn $ "ventMap = \n" ++ show ventMap ++ "\n" ++ prettyVentMap ventMap
    let dangerPoints = Map.filter (> 1) ventMap
    putStrLn $ "dangerPoints = \n" ++ show dangerPoints ++ "\nThere are " ++ show (length dangerPoints) ++ " danger points."

chunks :: Int -> [a] -> [[a]]
chunks x xs = 
    if length xs <= x 
        then [xs]
        else take x xs : (chunks x (drop x xs))

prettyVentMap :: Map.Map Point Int -> String
prettyVentMap m = intercalate "\n" (chunks (maxX + 1) ps)
    where
        countSymbol :: Int -> String
        countSymbol x = if x == 0 then "." else show x
        pointToSymbol :: Point -> String
        pointToSymbol p = countSymbol $ Map.findWithDefault 0 p m

        points :: [Point]
        points = map fst $ Map.toList m
        maxX :: Int
        maxX = maxInList $ map x points
        maxY :: Int
        maxY = maxInList $ map y points
        coords :: [Point]
        coords = [Point x y | y <- [0..maxX], x <- [0..maxY]]

        ps :: String
        ps = concat $ map pointToSymbol coords

maxInList :: Ord a => [a] -> a
maxInList [a] =  a
maxInList (a : rest) = max a (maxInList rest)

pointsInLine :: Line -> [Point]
pointsInLine (Line (Point x1 y1) (Point x2 y2)) = 
    if x1 == x2 && y1 == y2
    then [Point x2 y2]
    else Point x1 y1 : pointsInLine (Line (Point xNext yNext) (Point x2 y2))
    where 
        xNext = if x1 == x2 then x1 else x1 + xDirection
        xDirection = if abs x2 - x1 > 0 then 1 else -1
        yNext = if y1 == y2 then y1 else y1 + yDirection
        yDirection = if abs y2 - y1 > 0 then 1 else -1

buildVentMap :: [Line] -> Map.Map Point Int
buildVentMap lines = foldl folder Map.empty points
    where 
        folder :: Map.Map Point Int -> Point -> Map.Map Point Int
        folder acc p = Map.insert p (old + 1) acc
            where
                old = (Map.findWithDefault 0 p acc)

        points :: [Point]
        points = concat $ map pointsInLine lines

isVertical :: Line -> Bool
isVertical (Line (Point x1 _) (Point x2 _)) = x1 == x2

isHorizontal :: Line -> Bool
isHorizontal (Line (Point _ y1) (Point _ y2)) = y1 == y2

parseVent :: String -> Line
parseVent l = Line a b
    where 
        tokens = T.splitOn (T.pack " -> ") (T.pack l)
        x = map T.unpack $ T.splitOn (T.pack ",") (tokens !! 0)
        y = map T.unpack $ T.splitOn (T.pack ",") (tokens !! 1)
        x1 :: Int
        x1 = read $ x !! 0
        y1 :: Int
        y1 = read $ x !! 1
        x2 :: Int
        x2 = read $ y !! 0
        y2 :: Int
        y2 = read $ y !! 1
        a = Point x1 y1
        b = Point x2 y2