module Main where

import System.Environment (getArgs)
import qualified MyLib

main :: IO ()
main = do
  args <- getArgs
  if not $ length args == 2
     then putStrLn "Usage: advent-of-code <day> <input file>"
     else do
        input <- readFile (args !!1)
        MyLib.main (head args) input
