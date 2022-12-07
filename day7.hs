module Day7 where

import Data.Char (isSpace)
import qualified Data.List as List
import System.Environment (getArgs)

main :: IO ()
main = do
  input <- getPuzzleInput
  case input of
    Nothing -> return ()
    Just input -> do
      let inLines = filter (not .null) $ lines input
      putStrLn "Day 7"
      let parsed = filter (not .isLs) $ parseTerminalOutput inLines
      print parsed
      -- let fs = scanl evalTermOutLine emptyFileTreeCursor parsed
      -- putStrLn $ unlines $ map (\(cmd, s) -> show cmd <> "\n" <> show s <> "\n" <> printTreeCursor s) (zip parsed (tail fs))
      let fs = foldl evalTermOutLine emptyFileTreeCursor parsed
      putStrLn $ printTreeCursor fs
      let sizes = getDirSizes (tree fs)
      print sizes
      print $ sum $ filter (< 100000) sizes
      return ()

getDirSizes :: FileTree -> [Int]
getDirSizes (File _ _) = []
getDirSizes (Dir d children) = (treeSize (Dir d children)) : concatMap getDirSizes children

emptyFileTreeCursor = FileTreeCursor { tree = Dir "/" [], cd = [] }

treeSize :: FileTree -> Int
treeSize (File size _) = size
treeSize (Dir d children) = sum $ map treeSize children

data FileTree = Dir String [FileTree] | File Int String
  deriving (Show, Eq)

instance Ord FileTree where
  compare (Dir d _ ) (Dir d' _) = compare d d'
  compare (Dir d _ ) (File _ f) = compare d f
  compare (File _ f) (Dir d _ )= compare d f
  compare (File _ f) (File _ f')= compare f f'

showTree :: Int -> FileTree -> String
showTree indent (File size name) =
  (map (const ' ') [0..indent*2]) <> "- " <> name <> " (file, size=" <> show size <> ")"
showTree indent (Dir name children) =
  (map (const ' ') [0..indent*2]) <> "- " <> name <> " (dir, size=" <> show (treeSize (Dir name children)) <> ")\n" <> unlines (map (showTree (indent + 1)) (List.sort children))

printTree :: FileTree -> String
printTree = showTree 0

data FileTreeCursor = FileTreeCursor { tree :: FileTree, cd :: [String] }
  deriving (Show)
printTreeCursor tc = "FileTreeCursor /" <> List.intercalate "/" (cd tc) <> "\n" <> printTree (tree tc)

evalTermOutLine :: FileTreeCursor -> TermOutLine -> FileTreeCursor
evalTermOutLine tc l =
  case l of
    CD ".." -> tc { cd = take (length (cd tc) - 1) (cd tc) }
    CD "/" -> tc { cd = [] }
    CD dir -> tc { cd = cd tc  ++ [dir]}
    DirD dir -> tc { tree = insert (Dir dir []) (cd tc) (tree tc) }
    FileD size name -> tc { tree = insert (File size name) (cd tc) (tree tc) }
    _ -> tc

insert :: FileTree -> [String] -> FileTree -> FileTree
insert t' [] (Dir cdname children) = Dir cdname (children ++ [t'])
insert t' path (Dir cdname children) = (Dir cdname newChildren)
  where
    newChildren = newChild ++ others
    isTargetDir = isDirNamed (head path)
    others = filter (not . isTargetDir) children
    child = List.find isTargetDir children
    newChild = maybe [] updateChild child
    updateChild child = [insert t' (tail path) child]

isDirNamed :: String -> FileTree -> Bool
isDirNamed dirName (File _ _ ) = False
isDirNamed dirName (Dir dirName' _) = dirName == dirName'

data TermOutLine = CD String | LS | DirD String | FileD Int String | Unknown String deriving (Show)

isLs :: TermOutLine -> Bool
isLs LS = True
isLs _ = False

parseTerminalOutput :: [String] -> [TermOutLine]
parseTerminalOutput = map parseTermLine

parseTermLine :: String -> TermOutLine
parseTermLine l =
  case take 4 l of
    "$ cd" -> CD (drop 5 l)
    "$ ls" -> LS
    "dir " -> DirD (drop 4 l)
    _ -> FileD (read fileSize) fileName
  where
    (fileSize, (_:fileName)) = span (not . isSpace) l


getPuzzleInput :: IO (Maybe String)
getPuzzleInput = do
  args <- getArgs
  if length args /= 1 then do
    putStrLn $ "Please provide a filepath as input!"
    return Nothing
  else do
    let inPath = args !! 0
    putStrLn $ "reading input from " <> show (inPath)
    input <- readFile inPath
    putStrLn $ "---- Begin Puzzle Input ----\n" <> input <> "\n---- End Puzzle Input ----"
    return (Just input)

