module DayTwo (main) where

main :: String -> IO ()
main input = do
  let commands = map parseCommand $ lines input
  putStrLn $ show commands
  let targetP = foldl foldCommand initialPosition commands
  putStrLn $ show targetP
  let result = depth targetP * horizontal targetP
  putStrLn $ show result

data Command
  = Forward Int
  | Down Int
  | Up Int
  | Unknown
  deriving (Show)

data Position = Position { depth :: Int, horizontal :: Int } deriving (Show)

initialPosition :: Position
initialPosition = Position 0 0

foldCommand :: Position -> Command -> Position
foldCommand p cmd =
  case cmd of
    Forward x -> p { horizontal = x + horizontal p }
    Down x -> p { depth = x + depth p }
    Up x -> p { depth = depth p - x }
    Unknown -> p

parseCommand :: String -> Command
parseCommand line = cmd
  where tokens = words line
        cmd = case tokens !! 0 of
                "forward" -> Forward amount
                "down" -> Down amount
                "up" -> Up amount
                _ -> Unknown
        amount = read (tokens !! 1)
