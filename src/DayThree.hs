module DayThree (main) where

main :: String -> IO ()
main input = do
  let readings = map parseLine $ lines input
  putStrLn $ show readings
  let cols = transpose readings
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
  -- part two
  let (oxygenBit, co2Bit) = calcRatingBits (cols !! 0)
  putStrLn $ show (oxygenBit, co2Bit)
  let (oxygen, co2) = findRatings (readings, readings) 0
  print (oxygen, co2)
  let oxygenDec = interpBinary oxygen
  let co2Dec = interpBinary co2
  putStrLn $ "oxygen = " ++ show oxygenDec ++ ", co2 = " ++ show co2Dec ++ ", life support = " ++ show (oxygenDec * co2Dec)

-- this is so gross, i tried to do both in a single recursive pass but that was dumb, now transposing and calcing bits twice anyway /shrug
findRatings :: ([[Int]], [[Int]]) -> Int -> ([Int], [Int])
findRatings (oxygenReadings, co2Readings) i = 
  case (length oxygenReadingsLeft, length co2ReadingsLeft) of
    (1, 1) -> (head oxygenReadingsLeft, head co2ReadingsLeft)
    _ -> findRatings (oxygenReadingsLeft, co2ReadingsLeft) (i + 1)
  where colsOxy = transpose oxygenReadings
        colsCo2 = transpose co2Readings
        (oxygenCrit, _) = calcRatingBits (colsOxy !! i)
        (_, co2Crit) = calcRatingBits (colsCo2 !! i)
        oxygenReadingsLeft = 
          if length oxygenReadings == 1
            then oxygenReadings
            else filter (\x -> x !! i == oxygenCrit) oxygenReadings
        co2ReadingsLeft = 
          if length co2Readings == 1
            then co2Readings
            else filter (\x -> x !! i == co2Crit) co2Readings

transpose readings = 
  map (\i -> map (\xs -> xs !! i) readings) [0..(length (head readings) - 1)]

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
