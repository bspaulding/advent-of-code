module DaySeven (main) where

import qualified Data.List as List
import qualified Data.Text as T

main :: String -> IO ()
main input = do
    let crabs = parse input
    putStrLn $ "crabs = " ++ show crabs
    let costs = map (\p -> (p, calcCost crabs p)) [0..(maxInList crabs)]
    putStrLn $ "costs = " ++ show costs
    let (position, minCost) = head $ List.sortBy (\a b -> if (snd a) > (snd b) then GT else LT) costs
    putStrLn $ "min cost = " ++ show minCost ++ ", target position = " ++ show position

calcCost :: [Int] -> Int -> Int
calcCost positions targetPosition =
    sum costs
    where 
        costs = [factorial $ abs (targetPosition - position) | position <- positions]

factorial :: Int -> Int
factorial 0 = 0
factorial x = x + factorial (x - 1)

maxInList :: Ord a => [a] -> a
maxInList [a] =  a
maxInList (a : rest) = max a (maxInList rest)

parse :: String -> [Int]
parse s = map read ss
    where
        ss = map T.unpack $ T.splitOn (T.pack ",") (T.pack s)