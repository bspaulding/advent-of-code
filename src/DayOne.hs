module DayOne (main) where

main :: String -> IO ()
main input = do
  let depths = readDepths $ lines input
  putStrLn $ show depths
  let pairs = zip depths (drop 1 depths)
  let increases = length $ filter isIncrease pairs
  putStrLn $ show increases

isIncrease:: (Int, Int) -> Bool
isIncrease (x,y) = y > x

readDepths :: [String] -> [Int]
readDepths = map read
