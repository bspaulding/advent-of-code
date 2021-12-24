module DaySeventeen (main) where

import qualified Data.List as List

main :: String -> IO ()
main input = do
  let vs = [(7, 2), (6, 3), (6, 8), (6, 9), (6, 10)]
  putStrLn $ List.intercalate "\n" $ map (\p -> show p ++ " => " ++ show (freefallPos p)) vs

  let targetBox = parseInput input
  print targetBox

  print $ map (\p -> (p, inBox targetBox p)) vs

  let path = iterate step ((0, 0), (7,2))
  let steps = map fst $ take 10 path
  --print $ zip steps $ map (inBox targetBox) steps
  --print $ zip steps $ map (beyondBox targetBox) steps
  let stepsUntilBeyond = map fst $ takeWhile (not . (beyondBox targetBox) . fst) path
  print $ stepsUntilBeyond
  print $ any (inBox targetBox) steps

triangle n = (n * (n + 1)) `div` 2

freefallPos (vx, vy) = (tx, y, typos)
  where
    tx = triangle vx
    y = typos - tyneg
    typos = triangle vy
    tyneg = triangle (vx - vy - 1)

type Velocity = (Int, Int)

step :: (Point, Velocity) -> (Point, Velocity)
step ((x, y), (vx, vy)) = ((x', y'), (vx', vy'))
  where
    x' = x + vx
    y' = y + vy
    vx' = max 0 (vx - 1)
    vy' = vy - 1

inBox :: Box -> Point -> Bool
inBox ((x1, y1), (x2, y2), (x3, y3), (x4, y4)) (xp, yp) =
  xp >= xmin && xp <= xmax && yp >= ymin && yp <= ymax
  where
    xs = [x1, x2, x3, x4]
    xmin = minimum xs
    xmax = maximum xs
    ys = [y1, y2, y3, y4]
    ymin = minimum ys
    ymax = maximum ys

beyondBox :: Box -> Point -> Bool
beyondBox ((x1, y1), (x2, y2), (x3, y3), (x4, y4)) (xp, yp) =
  xp > xmax || yp < ymin
  where
    xs = [x1, x2, x3, x4]
    xmax = maximum xs
    ys = [y1, y2, y3, y4]
    ymin = minimum ys

type Box = (Point, Point, Point, Point)

type Point = (Int, Int)

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
