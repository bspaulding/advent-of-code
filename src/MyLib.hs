module MyLib (main) where

import qualified DayOne
import qualified DayTwo
import qualified DayThree
import qualified DayFour
import qualified DayFive
import qualified DaySix
import qualified DaySeven

main :: String -> String -> IO ()
main day input = do
  case day of
    "one" -> DayOne.main input
    "two" -> DayTwo.main input
    "three" -> DayThree.main input
    "four" -> DayFour.main input
    "five" -> DayFive.main input
    "six" -> DaySix.main input
    "seven" -> DaySeven.main input
    _ -> putStrLn $ "Unknown day '" ++ day ++ " "

