module DayFour (main) where

import qualified DayThree (transpose)
import Data.List
import qualified Data.Text as T

main :: String -> IO ()
main input = do
    let drawsStrings : _ : boardLines = lines input
    let draws = map read $ map T.unpack $ T.splitOn (T.pack ",") (T.pack drawsStrings)
    let boards = parseBoards boardLines []
    let (markedBoards, draw : draws') = findFirstWin (boards, draws)
    putStrLn $ prettyBoards markedBoards
    let winningBoard = head $ filter boardIsWon markedBoards
    putStrLn $ "winning board = \n" ++ prettyBoard winningBoard ++ "\n winning draw = " ++ show draw
    let score = boardScore winningBoard
    putStrLn $ "board score = " ++ show score
    let final = score * draw
    putStrLn $ "final score = " ++ show final

    let (lastWinBoard : _, lastWinDraw : _) = findFirstWin $ findLastWin (boards, draws)
    putStrLn $ "last winning board = \n" ++ prettyBoard lastWinBoard ++ "\n last winning draw = " ++ show lastWinDraw
    let lastWinScore = boardScore lastWinBoard
    let lastWinFinal = lastWinScore * lastWinDraw
    putStrLn $ "board score = " ++ show lastWinScore ++ ", last win final = " ++ show lastWinFinal
    

type Board = [[(Int, Bool)]]

boardScore :: Board -> Int
boardScore board = foldl (+) 0 (map fst unmarkedCells)
    where
        unmarkedCells = filter (not . snd) $ concat board

findLastWin :: ([Board], [Int]) -> ([Board], [Int])
findLastWin (boards, draws) = if done then (losers, draw : draws') else findLastWin (boards', draws')
    where
        draw = head draws
        boards' = markBoards boards draw
        draws' = tail draws
        losers = filter (not . boardIsWon) boards'
        done = length losers == 1 || length draws' == 0

findFirstWin :: ([Board], [Int]) -> ([Board], [Int])
findFirstWin (boards, draws) = if done then (boards', draw : draws') else findFirstWin (boards', draws')
    where
        draw = head draws
        boards' = markBoards boards draw
        draws' = tail draws
        wins = filter boardIsWon boards'
        done = length wins > 0 || length draws' == 0

boardIsWon :: Board -> Bool
boardIsWon board = anyRowComplete || anyColComplete
    where 
        completeRows = filter allMarked board
        anyRowComplete = length completeRows > 0

        cols = transpose board
        completeCols = filter allMarked cols
        anyColComplete = length completeCols > 0

        allMarked :: [(Int, Bool)] -> Bool
        allMarked = all snd

markBoards :: [Board] -> Int -> [Board]
markBoards boards draw = map (\b -> markBoard b draw) boards

markBoard :: Board -> Int -> Board
markBoard board draw = map markLine board
    where 
        markLine :: [(Int, Bool)] -> [(Int, Bool)]
        markLine xs = map markCell xs

        markCell :: (Int, Bool) -> (Int, Bool)
        markCell (x, marked) = if x == draw then (x, True) else (x, marked)

parseBoards :: [String] -> [Board] -> [Board]
parseBoards ls rest' = 
    if length ls == 0 
    then rest'
    else parseBoards rest (rest' ++ [board])
    where 
        boardLines :: [String]
        boardLines = take 5 ls

        board :: Board
        board = map readBoardLine boardLines

        readBoardLine :: String -> [(Int, Bool)]
        readBoardLine line = map (\w -> (read w, False)) (words line)

        rest :: [String]
        rest = drop 6 ls

prettyBoards :: [Board] -> String
prettyBoards boards = intercalate "\n\n" $ map prettyBoard boards

prettyBoard :: Board -> String
prettyBoard board = 
    intercalate "\n" $ map boardLine board
    where 
        boardLine :: [(Int, Bool)] -> String
        boardLine xs = intercalate "\t" (map showCell xs)

        showCell :: (Int, Bool) -> String
        showCell (x, marked) = if marked then "[" ++ show x ++ "]" else show x