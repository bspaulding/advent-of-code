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
    doStep conf 1
    doStep conf 2
    doStep conf 3
    doStep conf 4
    doStep conf 10

doStep :: Config -> Int -> IO ()
doStep conf n = do
  let rules = insertionRules conf
  let stepN = steps rules (pairs $ polymerTemplate conf) n
  print stepN
  let stepNCombined = combined stepN
  putStrLn $ stepNCombined
  let counts = Map.toList $ freqs stepNCombined
  let ((cmax, xmax), (cmin, xmin)) = (maximumBy (comparing snd) counts, minimumBy (comparing snd) counts)
  print ((cmax, xmax), (cmin, xmin))
  let diff = xmax - xmin
  putStrLn $ "difference at step " ++ show n ++ " = " ++ show diff

freqs :: Ord a => [a] -> Map.Map a Int
freqs = foldl getAndInc Map.empty
  where
    getAndInc acc x = Map.insert x (1 + Map.findWithDefault 0 x acc) acc

pairs :: String -> [(Char, Char)]
pairs s = zip s (drop 1 s)

combined :: [(Char, Char)] -> String
combined [] = ""
combined (p0 : pairs) = foldl (++) [fst p0] (map (\p -> [snd p]) (p0 : pairs))

steps :: InsertionRules -> [(Char, Char)] -> Int -> [(Char, Char)]
steps rules cs n = foldl (\acc _ -> step rules acc) cs [0 .. (n - 1)]

step :: InsertionRules -> [(Char, Char)] -> [(Char, Char)]
step rules = concatMap (insertion rules)

insertion :: InsertionRules -> (Char, Char) -> [(Char, Char)]
insertion rules (c1, c2) =
  case Map.lookup [c1, c2] rules of
    Nothing -> [(c1, c2)]
    Just c3 -> [(c1, c3), (c3, c2)]

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