module DayThirteen (main) where

import qualified Data.Set as Set
import qualified Data.Text as Text
import Text.Parsec
import Text.Parsec.Token

join :: String -> [String] -> String
join sep xs = Text.unpack $ Text.intercalate (Text.pack sep) (map Text.pack xs)

splitOn :: String -> String -> [String]
splitOn sep s = map Text.unpack $ Text.splitOn (Text.pack sep) (Text.pack s)

type Point = (Int, Int)

main :: String -> IO ()
main input =
  case parseInput input of
    Left err -> print err
    Right (points, folds) -> do
      putStrLn $ "points = " ++ show points
      putStrLn $ "folds = " ++ show folds
      --   putStrLn $ drawMap points
      let firstFold = foldPaper (head folds) points
      putStrLn $ "first fold = " ++ show firstFold
      --   putStrLn $ drawMap firstFold
      putStrLn $ show (length firstFold) ++ " dots are visible after the first fold."

--   let secondFold = foldPaper (folds !! 1) firstFold
--   putStrLn $ "second fold = " ++ show secondFold
--   putStrLn $ drawMap secondFold
--   putStrLn $ show (length secondFold) ++ " dots are visible after the second fold."

foldPaper :: Point -> [Point] -> [Point]
foldPaper p ps = Set.toList $ Set.fromList $ map (foldPoint p) ps

foldPoint :: Point -> Point -> Point
foldPoint (yf, 0) (y, x) =
  if y > yf
    then (abs (y - yf - yf), x)
    else (y, x)
foldPoint (0, xf) (y, x) =
  if x > xf
    then (y, abs (x - xf - xf))
    else (y, x)
foldPoint (yf, xf) (y, x) = (y, x)

drawMap :: [Point] -> String
drawMap ps = join "\n" (chunks (width + 1) dots)
  where
    height = maximum (map fst ps)
    width = maximum (map snd ps)
    coords = [(y, x) | y <- [0 .. height], x <- [0 .. width]]
    dots = map (\p -> if p `elem` ps then '#' else '.') coords

chunks :: Int -> [a] -> [[a]]
chunks x xs =
  if length xs <= x
    then [xs]
    else take x xs : (chunks x (drop x xs))

-- here be parsing stuff, I overcomplicated this to play with parsec

parseInput = parse inputParser "error?"

inputParser = do
  points <- endBy point endOfLine
  _ <- char '\n'
  folds <- endBy parseFoldPoint endOfFold
  return (points, folds)

endOfFold = chomp (char '\n') <|> (notFollowedBy anyToken <?> "eof")

chomp p = do
  _ <- p
  return ()

point = do
  x <- int
  char ','
  y <- int
  return (y, x)

parseFoldPoint = try foldX <|> try foldY

foldX = do
  string "fold along x="
  x <- int
  return (0, x)

foldY = do
  string "fold along y="
  y <- int
  return (y, 0)

int = do
  xs <- many digit
  return (read xs :: Int)