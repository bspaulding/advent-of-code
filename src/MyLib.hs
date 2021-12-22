module MyLib (main) where

import qualified DayEight
import qualified DayEleven
import qualified DayFifteen
import qualified DayFive
import qualified DayFour
import qualified DayFourteen
import qualified DayNine
import qualified DayOne
import qualified DaySeven
import qualified DaySix
import qualified DayTen
import qualified DayThirteen
import qualified DayThree
import qualified DayTwelve
import qualified DayTwo
import qualified DaySixteen

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
    "eleven" -> DayEleven.main input
    "twelve" -> DayTwelve.main input
    "thirteen" -> DayThirteen.main input
    "fourteen" -> DayFourteen.main input
    "fifteen" -> DayFifteen.main input
    "sixteen" -> DaySixteen.main input
    _ -> putStrLn $ "Unknown day '" ++ day ++ " "
