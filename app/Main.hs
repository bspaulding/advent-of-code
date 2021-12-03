module Main where

import System.Environment (getArgs)
import qualified MyLib
import qualified DayOne

main :: IO ()
main = do
  args <- getArgs
  if length args == 0
     then putStrLn "Please specify a day..."
     else
        case head args of
         "one" -> DayOne.main
         _ -> MyLib.someFunc
