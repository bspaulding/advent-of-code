module DaySix (main) where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as T

main :: String -> IO ()
main input = do
    let initialPop = parsePop input
    putStrLn $ "initial population = " ++ show initialPop
    let freqs = frequency initialPop
    putStrLn $ "initial freqs = " ++ show freqs
    putStrLn $ "80 days (not brute) => " ++ (show $ foldl (+) 0 $ calcDays freqs 80)
    putStrLn $ "256 days (not brute) => " ++ (show $ foldl (+) 0 $ calcDays freqs 256)

frequency :: [Int] -> [Int]
frequency xs = freqs
    where
       p = List.sort xs
       counts = foldl (\acc age -> Map.insert age (1 + Map.findWithDefault 0 age acc) acc) Map.empty p 
       freqs = map (\i -> Map.findWithDefault 0 i counts) [0..8]


calcDays :: [Int] -> Int -> [Int]
calcDays p 0 = p
calcDays (p0 : p1 : p2 : p3 : p4 : p5 : p6 : p7 : p8 : []) d = 
    calcDays (p1 : p2 : p3 : p4 : p5 : p6 : p0 + p7 : p8 : p0 : []) (d - 1)

parsePop :: String -> [Int]
parsePop s = map read ss
    where
        ss = map T.unpack $ T.splitOn (T.pack ",") (T.pack s)