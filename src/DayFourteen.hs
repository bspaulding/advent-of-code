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
    let rules = insertionRules conf
    let step10 = steps rules (polymerTemplate conf) 10
    let counts = Map.toList $ freqs step10
    let ((cmax, xmax), (cmin, xmin)) = (maximumBy (comparing snd) counts, minimumBy (comparing snd) counts)
    print ((cmax, xmax), (cmin, xmin))
    putStrLn $ "Difference = " ++ show (xmax - xmin)

freqs :: Ord a => [a] -> Map.Map a Int
freqs = foldl getAndInc Map.empty
  where
    getAndInc acc x = Map.insert x (1 + Map.findWithDefault 0 x acc) acc

steps :: Map.Map String Char -> String -> Int -> String
steps rules cs n = foldl (\acc _ -> step rules acc) cs [0 .. (n - 1)]

step :: Map.Map String Char -> String -> String
step rules cs = combined
  where
    combined = foldl (\acc cs -> acc ++ drop 1 cs) (head inserted) (tail inserted)
    inserted = map (insertion rules) (zip cs (drop 1 cs))

insertion :: Map.Map String Char -> (Char, Char) -> String
insertion rules (c1, c2) =
  case Map.lookup [c1, c2] rules of
    Nothing -> [c1, c2]
    Just c3 -> [c1, c3, c2]

-- parsing

data Config = Config {polymerTemplate :: String, insertionRules :: Map.Map String Char} deriving (Show)

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