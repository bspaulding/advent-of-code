module Main where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified MyLib
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  if length args /= 2
    then putStrLn "Usage: advent-of-code [scaffold] <day> [<input file>]"
    else case head args of
      "scaffold" -> scaffoldDay (args !! 1)
      _ -> do
        input <- readFile (args !! 1)
        MyLib.main (head args) input

scaffoldDay :: String -> IO ()
scaffoldDay day = do
  _ <- writeFile srcFilePath srcFileContents
  _ <- writeFile testFilePath ""
  cabalFileContents <- readFile cabalFilePath
  length cabalFileContents `seq` writeFile cabalFilePath (addOtherModule camelName cabalFileContents)
  entryPointContents <- readFile entryPointFilePath
  length entryPointContents `seq` writeFile entryPointFilePath (addDayCase day camelName entryPointContents)
  return ()
  where
    camelName = "Day" ++ [Char.toUpper (head day)] ++ tail day
    srcFilePath = "src/" ++ camelName ++ ".hs"
    srcFileContents = "module " ++ camelName ++ " (main) where\n\nmain :: String -> IO ()\nmain input = do\n  putStrLn \"" ++ camelName ++ "\""
    testFilePath = "day-" ++ day ++ "-test.txt"
    cabalFilePath = "advent-of-code.cabal"
    entryPointFilePath = "src/MyLib.hs"

addOtherModule :: String -> String -> String
addOtherModule moduleName cabalFileContents =
  unlines $ map transformLine $ lines cabalFileContents
  where
    transformLine line =
      if "other-modules: " `List.isInfixOf` line
        then line ++ ", " ++ moduleName
        else line

addDayCase :: String -> String -> String -> String
addDayCase day moduleName contents =
  unlines $ header ++ imports ++ [newImport] ++ newMain
  where
    ls = lines contents
    header = take 2 ls
    imports = filter (\line -> "import qualified " `List.isInfixOf` line) ls
    newImport = "import qualified " ++ moduleName
    newMain = map transformLine $ drop (2 + length imports) ls
    transformLine line =
      if "_ -> " `List.isInfixOf` line
        then "    \"" ++ day ++ "\" -> " ++ moduleName ++ ".main input\n" ++ line
        else line