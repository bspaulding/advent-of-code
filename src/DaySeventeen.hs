module DaySeventeen (main) where

import qualified Data.List as List

main :: String -> IO ()
main input = do
  let ps = [(7, 2), (6, 3), (6, 8), (6, 9), (6, 10)]
  putStrLn $ List.intercalate "\n" $ map (\p -> show p ++ " => " ++ show (endPos p)) ps

triangle n = (n * (n + 1)) `div` 2

endPos (vx, vy) = (tx, y, typos)
  where
    tx = triangle vx
    y = typos - tyneg
    typos = triangle vy
    tyneg = triangle (vx - vy - 1)