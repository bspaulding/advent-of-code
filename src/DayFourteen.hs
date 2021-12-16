module DayFourteen (main) where

import qualified Control.Monad as Monad
import Data.List
import qualified Data.Map as Map
import Data.Ord
import Text.Parsec
import Text.Parsec.Token

main :: String -> IO ()
main input = case parseInput input of
  Left err -> print err
  Right conf -> do
    print conf
    doStep conf 10

doStep :: Config -> Int -> IO ()
doStep conf n = do
  let rules = insertionRules conf
  let stepN = steps rules (polymerTemplate conf) n
  let counts = Map.toList $ freqs stepN
  let ((cmax, xmax), (cmin, xmin)) = (maximumBy (comparing snd) counts, minimumBy (comparing snd) counts)
  print ((cmax, xmax), (cmin, xmin))
  let diff = xmax - xmin
  putStrLn $ "difference at step " ++ show n ++ " = " ++ show diff

freqs :: Ord a => [a] -> Map.Map a Int
freqs = foldl getAndInc Map.empty
  where
    getAndInc acc x = Map.insert x (1 + Map.findWithDefault 0 x acc) acc

steps :: InsertionRules -> String -> Int -> String
steps rules cs n = foldl (\acc _ -> step rules acc) cs [0 .. (n - 1)]

step :: InsertionRules -> String -> String
step rules cs = combined
  where
    combined = foldl1 (\acc cs -> acc ++ drop 1 cs) inserted
    inserted = zipWith (curry (insertion rules)) cs (drop 1 cs)

insertion :: InsertionRules -> (Char, Char) -> String
insertion rules (c1, c2) =
  case Map.lookup [c1, c2] rules of
    Nothing -> [c1, c2]
    Just c3 -> [c1, c3, c2]

-- parsing

type InsertionRules = Map.Map String Char

data Config = Config {polymerTemplate :: String, insertionRules :: InsertionRules} deriving (Show)

parseInput :: String -> Either ParseError Config
parseInput = parse inputParser "error"

inputParser = do
  polymerTemplate <- many upper
  _ <- char '\n'
  _ <- char '\n'
  insertionRules <- parseInsertionRule `endBy` (Monad.void endOfLine <|> eof)
  return $ Config {polymerTemplate = polymerTemplate, insertionRules = Map.fromList insertionRules}

parseInsertionRule = do
  cs <- many upper
  _ <- string " -> "
  c <- upper
  return (cs, c)