module Main where

import System.Environment (getArgs)
import qualified MyLib
import qualified DayOne

main :: IO ()
main = do
  args <- getArgs
  if not $ length args == 2
     then putStrLn "Usage: advent-of-code <day> <input file>"
     else do
        input <- readFile (args !!1)
        case head args of
         "one" -> DayOne.main input
         _ -> MyLib.someFunc
