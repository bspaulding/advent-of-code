module DayThree (main) where

main :: String -> IO ()
main input = do
  let readings = map parseLine $ lines input
  putStrLn $ show readings
  let cols = map (\i -> map (\xs -> xs !! i) readings) [0..(length (head readings) - 1)]
  putStrLn $ show cols
  let gammaBits = map calcGammaBit cols
  putStrLn $ show gammaBits
  let gamma = interpBinary gammaBits
  putStrLn $ show gamma
  let epsilonBits = map (\x -> if x == 1 then 0 else 1) gammaBits
  putStrLn $ show epsilonBits
  let epsilon = interpBinary epsilonBits
  putStrLn $ show epsilon
  let power = gamma * epsilon
  putStrLn $ show power
  let (oxygenBit, co2Bit) = calcRatingBits (head cols)
  putStrLn $ show (oxygenBit, co2Bit)

interpBinary :: [Int] -> Int
interpBinary bits' = x
  where bits = reverse bits'
        bitsI = zip bits [0..]
        pows = map (\(b, i) -> b * (2 ^ i)) bitsI
        x = foldl (+) 0 pows

calcRatingBits :: [Int] -> (Int, Int)
calcRatingBits xs = (oxygen, co2)
  where zeros = length $ filter (== 0) xs
        ones = length $ filter (== 1) xs
        oxygen = if zeros == ones then 1
                 else if zeros > ones then 0 else 1
        co2 = if zeros == ones then 0
                 else if zeros > ones then 1 else 0

calcGammaBit :: [Int] -> Int
calcGammaBit xs = gamma
  where x = foldl (+) 0 xs
        l = length xs
        t = (realToFrac l) / 2.0
        gamma = if (realToFrac x) > t then 1 else 0

parseLine :: String -> [Int]
parseLine = map (\x -> read [x])
