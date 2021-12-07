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
        costs = [triangular $ abs (targetPosition - position) | position <- positions]

triangular :: Int -> Int
triangular x = x * (x + 1) `div` 2

maxInList :: Ord a => [a] -> a
maxInList [a] =  a
maxInList (a : rest) = max a (maxInList rest)

parse :: String -> [Int]
parse s = map read ss
    where
        ss = map T.unpack $ T.splitOn (T.pack ",") (T.pack s)