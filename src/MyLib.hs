module MyLib (main) where

import qualified DayOne
import qualified DayTwo
import qualified DayThree

main :: String -> String -> IO ()
main day input = do
  case day of
    "one" -> DayOne.main input
    "two" -> DayTwo.main input
    "three" -> DayThree.main input
    _ -> putStrLn $ "Unknown day '" ++ day ++ " "

