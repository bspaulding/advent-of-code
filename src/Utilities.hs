module Utilities where

import qualified Data.Text as Text

join :: String -> [String] -> String
join sep xs = Text.unpack $ Text.intercalate (Text.pack sep) (map Text.pack xs)

splitOn :: String -> String -> [String]
splitOn sep s = map Text.unpack $ Text.splitOn (Text.pack sep) (Text.pack s)