module DayTen (main) where

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text

main :: String -> IO ()
main input = do
  let subsytems = lines input
  let results = map (\s -> isCorrupted s []) subsytems
  putStrLn $ join "\n" $ map show (zip subsytems results)

  let score = sum (map charScore (Maybe.catMaybes results))
  putStrLn $ "score = " ++ show score

isCorrupted :: String -> [Char] -> Maybe Char
isCorrupted [] stack = Nothing -- if null stack then Nothing else Just '!'
isCorrupted chars stack =
  case newStack of
    Right newStack -> isCorrupted (tail chars) newStack
    Left illegal -> Just illegal
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