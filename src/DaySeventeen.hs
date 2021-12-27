module DaySeventeen (main) where

import qualified Data.List as List
import qualified Data.Ord as Ord

main :: String -> IO ()
main input = do
  let targetBox = parseInput input
  print targetBox
  putStrLn $ "minVx = " ++ show (minVx targetBox) ++ ", maxVx = " ++ show (maxVx targetBox) ++ ", minVy = " ++ show (minVy targetBox) ++ ", maxVy = " ++ show (maxVy targetBox)
  let maxY = findMaxY targetBox
  putStrLn $ "max y = " ++ show maxY
  let paths = allValidPaths targetBox
  putStrLn $ "there are " ++ show (length paths) ++ " valid initial velocities."

pathFrom :: Box -> Velocity -> [Point]
pathFrom box v = map fst $ stepsUntilBeyond box path
  where
    path = iterate step ((0, 0), v)

prettySteps box steps = List.intercalate "\n" $ map (prettyStep box) steps

prettyStep box (p, v) = "p = " ++ show p ++ ", v = " ++ show v ++ ", beyondBox? = " ++ show (beyondBox box p)

stepsUntilBeyond box = takeWhile (not . beyondBox box . fst)

minVx box = ceiling $ triangleRoot (fromInteger $ xmin box)

maxVx box = floor $ triangleRoot (fromInteger $ xmax box)

maxVy box = abs $ ymin box

minVy = ymin

findMaxY :: Box -> Integer
findMaxY box =
  snd $ List.maximumBy (Ord.comparing snd) $ concat (validPaths box)
  where
    vs = [(vx, vy) | vx <- [(minVx box) .. (maxVx box)], vy <- [(minVy box) .. (maxVy box)]]
    valid = any (inBox box)
    validPaths :: Box -> [[Point]]
    validPaths box = filter valid $ map (pathFrom box) vs

allValidPaths :: Box -> [[Point]]
allValidPaths box = filter valid $ map (pathFrom box) vs
  where
    -- phone it in here, i know the search space needs to be bigger, but by what limit?
    vs = [(vx, vy) | vx <- [0 .. 1000], vy <- [-1000 .. 1000]]
    valid = any (inBox box)

triangle n = (n * (n + 1)) `div` 2

triangleRoot x = (sqrt (8 * x + 1) - 1) / 2

freefallPos (vx, vy) = (tx, y, typos)
  where
    tx = triangle vx
    y = typos - tyneg
    typos = triangle vy
    tyneg = triangle (vx - vy - 1)

type Velocity = (Integer, Integer)

step :: (Point, Velocity) -> (Point, Velocity)
step ((x, y), (vx, vy)) = ((x', y'), (vx', vy'))
  where
    x' = x + vx
    y' = y + vy
    vx' = max 0 (vx - 1)
    vy' = vy - 1

inBox :: Box -> Point -> Bool
inBox b (xp, yp) =
  xp >= xmin b && xp <= xmax b && yp >= ymin b && yp <= ymax b

beyondBox :: Box -> Point -> Bool
beyondBox b (xp, yp) =
  xp > xmax b || yp < ymin b

xmin :: Box -> Integer
xmin ((x1, _), (x2, _), (x3, _), (x4, _)) =
  minimum [x1, x2, x3, x4]

xmax :: Box -> Integer
xmax ((x1, _), (x2, _), (x3, _), (x4, _)) =
  maximum [x1, x2, x3, x4]

ymax :: Box -> Integer
ymax ((_, y1), (_, y2), (_, y3), (_, y4)) =
  maximum [y1, y2, y3, y4]

ymin :: Box -> Integer
ymin ((_, y1), (_, y2), (_, y3), (_, y4)) =
  minimum [y1, y2, y3, y4]

type Box = (Point, Point, Point, Point)

type Point = (Integer, Integer)

parseInput :: String -> Box
parseInput s = ((x1, y1), (x1, y2), (x2, y1), (x2, y2))
  where
    ranges = drop 13 s
    xrange = drop 2 $ takeWhile (/= ',') ranges
    x1 = read $ takeWhile (/= '.') xrange
    x2 = read $ drop 2 $ dropWhile (/= '.') xrange
    yrange = drop 4 $ dropWhile (/= ',') ranges
    y1 = read $ takeWhile (/= '.') yrange
    y2 = read $ drop 2 $ dropWhile (/= '.') yrange