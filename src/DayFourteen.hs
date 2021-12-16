module DayFourteen (main) where

import qualified Control.Monad as Monad
import qualified Data.Bifunctor as Bifunctor
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
    doStep conf 40

doStep :: Config -> Int -> IO ()
doStep conf n = do
  let rules = insertionRules conf
  let template = polymerTemplate conf
  let stepN = steps rules (pairs template) n
  let counts = Map.toList $ foldl (Map.unionWith (+)) (Map.singleton (last template) 1) $ map (\(p, n) -> Map.singleton (fst p) n) stepN
  putStrLn $ "counts = " ++ show counts
  let ((cmax, xmax), (cmin, xmin)) = (maximumBy (comparing snd) counts, minimumBy (comparing snd) counts)
  print ((cmax, xmax), (cmin, xmin))
  let diff = xmax - xmin
  putStrLn $ "difference at step " ++ show n ++ " = " ++ show diff

freqsBy :: Ord b => (a -> b) -> [a] -> Map.Map b Int
freqsBy f = foldl getAndInc Map.empty
  where
    getAndInc acc x = Map.insert k (1 + Map.findWithDefault 0 k acc) acc
      where
        k = f x

pairs :: String -> [PairCount]
pairs s = map (\p -> (p, 1)) ps
  where
    ps = zip s (drop 1 s)

steps :: InsertionRules -> [PairCount] -> Int -> [PairCount]
steps rules cs n = foldl (\acc _ -> step rules acc) cs [0 .. (n - 1)]

step :: InsertionRules -> [PairCount] -> [PairCount]
step rules p0 = Map.toList $ foldl (Map.unionWith (+)) Map.empty $ map (uncurry Map.singleton) $ concatMap (insertion rules) p0

insertion :: InsertionRules -> PairCount -> [PairCount]
insertion rules ((c1, c2), nc) =
  case Map.lookup [c1, c2] rules of
    Nothing -> [((c1, c2), nc)]
    Just c3 -> [((c1, c3), nc), ((c3, c2), nc)]

-- parsing

type Pair = (Char, Char)

type PairCount = (Pair, Int)

type PairMap = Map.Map Pair Int

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