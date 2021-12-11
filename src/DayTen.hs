module DayTen (main) where

import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Tuple as Tuple

main :: String -> IO ()
main input = do
  let subsystems = lines input
  let results = map (`parse` []) subsystems
  putStrLn $ join "\n" $ zipWith (curry show) subsystems results

  let score = sum (map charScore (Either.lefts results))
  putStrLn $ "score = " ++ show score

  let uncorruptedSystems = filter (Either.isRight . snd) (zip subsystems results)
  putStrLn $ "valid systems = \n" ++ join "\n" (map (show . fst) uncorruptedSystems)

  let autocompleted = map (\(openings, Right stack) -> autocomplete openings stack) uncorruptedSystems
  putStrLn $ "autocompleted = \n" ++ join "\n" (map show autocompleted)

  let completions = map completion $ Either.rights $ map snd uncorruptedSystems
  let completionScores = map scoreCompletion completions
  putStrLn $ "completions = \n" ++ join "\n" (map show (zip completions completionScores))

  let middleScore = head $ drop (length completionScores `div` 2) $ List.sort completionScores
  putStrLn $ "middleScore = " ++ show middleScore

scoreCompletion :: String -> Int
scoreCompletion = foldl f 0
  where
    f acc c = acc * 5 + score c
    score c = case c of
      ')' -> 1
      ']' -> 2
      '}' -> 3
      '>' -> 4
      _ -> 0

completion :: String -> String
completion = concatMap getCloseTag
  where
    getCloseTag c =
      case Map.lookup c openingTags of
        Nothing -> ""
        Just c -> [c]

autocomplete :: String -> String -> String
autocomplete openings stack = openings ++ completion stack

parse :: String -> [Char] -> Either Char [Char]
parse [] stack = Right stack
parse chars stack =
  case newStack of
    Right newStack -> parse (tail chars) newStack
    Left illegal -> Left illegal
  where
    c :: Char
    c = head chars

    closingTag :: Maybe Char
    closingTag = Map.lookup c closingTags

    lastTag :: Char
    lastTag = head stack

    newStack :: Either Char [Char]
    newStack = case closingTag of
      Nothing -> Right (c : stack)
      Just openingTag ->
        if openingTag == lastTag
          then -- get everything but the last element of stack
            Right (tail stack)
          else Left c

closingTags :: Map.Map Char Char
closingTags = Map.fromList [(')', '('), (']', '['), ('}', '{'), ('>', '<')]

openingTags :: Map.Map Char Char
openingTags = Map.fromList $ map Tuple.swap $ Map.toList closingTags

charScore :: Char -> Int
charScore c =
  case c of
    ')' -> 3
    ']' -> 57
    '}' -> 1197
    '>' -> 25137
    _ -> 0

join :: String -> [String] -> String
join sep xs = Text.unpack $ Text.intercalate (Text.pack sep) (map Text.pack xs)