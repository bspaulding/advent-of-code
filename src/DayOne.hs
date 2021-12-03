module DayOne (main) where

main :: String -> IO ()
main input = do
  let depths = readDepths $ lines input
  putStrLn $ show depths
  let windows = zip3 depths (drop 1 depths) (drop 2 depths)
  let windowSums = map sumWindow windows
  let pairs = zip windowSums (drop 1 windowSums)
  let increases = length $ filter isIncrease pairs
  putStrLn $ show increases

isIncrease :: (Int, Int) -> Bool
isIncrease (x,y) = y > x

readDepths :: [String] -> [Int]
readDepths = map read

sumWindow :: (Int, Int, Int) -> Int
sumWindow (x, y, z) = x + y + z
