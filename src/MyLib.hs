module MyLib (main) where

import qualified DayEight
import qualified DayFive
import qualified DayFour
import qualified DayNine
import qualified DayOne
import qualified DaySeven
import qualified DaySix
import qualified DayTen
import qualified DayThree
import qualified DayTwo

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
    "eight" -> DayEight.main input
    "nine" -> DayNine.main input
    "ten" -> DayTen.main input
    _ -> putStrLn $ "Unknown day '" ++ day ++ " "
