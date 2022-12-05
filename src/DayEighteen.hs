module DayEighteen where

import Data.Either (fromRight)
import Data.List (intercalate)
import qualified Data.List as List
import Test.HUnit
import Text.Parsec
import Text.Parsec.Token
import Text.Read (readEither)

data SFDigit = Regular Integer | Pair SFDigit SFDigit deriving (Eq)

instance Num SFDigit where
  fromInteger x = Regular x

  (+) a b = Pair a b
  (*) a b = 0
  abs a = a
  negate a = a
  signum a = a

instance Show SFDigit where
  show (Regular x) = show x
  show (Pair a b) = "[" ++ show a ++ "," ++ show b ++ "]"

parseDigit :: String -> Either ParseError SFDigit
parseDigit = parse digitParser "error"

digitRegular = do
  d <- many1 digit
  return $ Regular (read d)

digitParser = do
  _ <- char '['
  digit1 <- digitRegular <|> digitParser
  _ <- char ','
  digit2 <- digitRegular <|> digitParser
  _ <- char ']'
  return $ Pair digit1 digit2

-- If any regular number is 10 or greater, the leftmost such regular number splits.
split :: SFDigit -> SFDigit
split (Pair x y) = Pair splitX (if splitX == x then splitY else y)
  where
    splitX = split x
    splitY = split y
split (Regular x) =
  if x >= 10
    then Pair (Regular $ x `div` 2) (Regular $ x `div` 2 + (x `mod` 2))
    else Regular x

explode :: SFDigit -> SFDigit
explode = fromList . explodeList . toList

type SnailList = [(String, Integer)]

{-
  Convert an SFDigit to a SnailList structure:

  * MyLib DayEighteen> p1
  [[[[[9,8],1],2],3],4]
  * MyLib DayEighteen> toList p1
  [("00000",9),("00001",8),("0001",1),("001",2),("01",3),("1",4)]
-}
toList :: SFDigit -> SnailList
toList = toListR ""

toListR :: String -> SFDigit -> SnailList
toListR prefix (Regular x) = [(prefix, x)]
toListR prefix (Pair a b) = a' ++ b'
  where
    a' = toListR (prefix ++ "0") a
    b' = toListR (prefix ++ "1") b

fromList :: SnailList -> SFDigit
fromList = foldl insert emptyPair

insert :: SFDigit -> (String, Integer) -> SFDigit
insert (Regular x) (_, _) = Regular x
insert (Pair a b) ([], x) = Pair a b
insert (Pair a b) ("0", x) = Pair (Regular x) b
insert (Pair a b) ("1", x) = Pair a (Regular x)
insert (Pair a b) (('0' : rest), x) = Pair (insert a' (rest, x)) b
  where
    a' = case a of
      Regular x -> emptyPair
      Pair _ _ -> a
insert (Pair a b) (('1' : rest), x) = Pair a (insert b' (rest, x))
  where
    b' = case b of
      Regular _ -> emptyPair
      Pair _ _ -> b

emptyPair :: SFDigit
emptyPair = Pair (Regular 0) (Regular 0)

-- If any pair is nested inside four pairs, the leftmost such pair explodes.
explodeList :: SnailList -> SnailList
explodeList ds =
  case explodeI of
    Nothing -> ds
    Just i -> addRight i $ addLeft i ds
  where
    explodeI = List.findIndex (\(p, x) -> length p == 5) ds
    addLeft i ds = take (i - 1) ds ++ newDigit ++ drop (i + 1) ds
      where
        newDigit = case i of
          0 -> []
          x ->
            let (dpath, d) = ds !! (i - 1)
                (_, d') = ds !! i
             in [(dpath, d + d')]
    addRight i ds =
      take i ds ++ newDigit ++ drop (i + 2) ds
      where
        newDigit =
          if i + 1 == length ds
            then []
            else
              let (_, d) = ds !! i
                  (dpath, d') = ds !! (i + 1)
               in [(dpath, d + d')]

p1 = Pair (Pair (Pair (Pair (Pair 9 8) 1) 2) 3) 4

p2 = Pair 7 (Pair 6 (Pair 5 (Pair 4 (Pair 3 2))))

tests =
  TestList
    [ TestLabel "parses" testParses,
      TestLabel "explodes" testExplodes,
      TestLabel "splits" testSplits,
      TestLabel "reduces" testReduces,
      TestLabel "adds" testAdds,
      TestLabel "sums" testSums
    ]

testParses =
  TestList
    [ TestCase $
        assertEqual
          "can parse"
          (Right p1)
          (parseDigit "[[[[[9,8],1],2],3],4]"),
      TestCase $
        assertEqual
          "can parse regular numbers >= 10"
          (Right (Pair (Regular 1) (Regular 10)))
          (parseDigit "[1,10]")
    ]

testExplodes =
  TestList $ map mkCase cases
  where
    cases =
      [ ("[[[[[9,8],1],2],3],4]", "[[[[0,9],2],3],4]"),
        ("[7,[6,[5,[4,[3,2]]]]]", "[7,[6,[5,[7,0]]]]"),
        ("[[6,[5,[4,[3,2]]]],1]", "[[6,[5,[7,0]]],3]"),
        ("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"),
        ("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[7,0]]]]")
      ]
    mkCase (a, b) =
      TestCase $
        assertEqual
          ("can explode " ++ show a ++ " into " ++ show b)
          b
          (show (explode aParsed))
      where
        aParsed = fromRight (Regular 0) (parseDigit a)

testSplits = TestList $ map mkCase cases
  where
    cases =
      [ -- ("[[[[0,7],4],[15,[0,13]]],[1,1]]", "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]"),
        ("[[[[0,7],4],[[7,8],[0,13]]],[1,1]]", "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]")
      ]
    mkCase (a, b) = TestCase $ assertEqual ("can split " ++ show a ++ "into " ++ show b) b (show (split aParsed))
      where
        aParsed = fromRight (Regular 0) (parseDigit a)

testReduces = TestList $ map mkCase cases
  where
    cases = [("[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]", "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")]
    mkCase (a, b) = TestCase $ assertEqual ("can reduce " ++ show a ++ " into " ++ show b) b (show (reduce aParsed))
      where
        aParsed = fromRight (Regular 0) (parseDigit a)

reduce :: SFDigit -> SFDigit
reduce d =
  if d == d'
    then d
    else reduce d'
  where
    d' = explode . split $ d

add :: SFDigit -> SFDigit -> SFDigit
add = Pair

testAdds = TestList $ map mkCase cases
  where
    cases = [("[[[[4,3],4],4],[7,[[8,4],9]]]", "[1,1]", "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")]
    mkCase (a, b, c) = TestCase $ assertEqual ("can add " ++ show a ++ " and " ++ show b ++ " into " ++ show c) c (show (add aParsed bParsed))
      where
        aParsed = fromRight (Regular 0) (parseDigit a)
        bParsed = fromRight (Regular 0) (parseDigit b)

sumDigits :: [SFDigit] -> SFDigit
sumDigits ds = reduce $ foldl1 (add . reduce) ds

testSums = TestList $ map mkCase cases
  where
    cases =
      [ (lines "[1,1]\n[2,2]\n[3,3]\n[4,4]", "[[[[1,1],[2,2]],[3,3]],[4,4]]"),
        (lines "[1,1]\n[2,2]\n[3,3]\n[4,4]\n[5,5]", "[[[[3,0],[5,3]],[4,4]],[5,5]]"),
        (lines "[1,1]\n[2,2]\n[3,3]\n[4,4]\n[5,5]\n[6,6]", "[[[[5,0],[7,4]],[5,5]],[6,6]]"),
        -- ( [ "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]",
        --     "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]",
        --     "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]",
        --     "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]",
        --     "[7,[5,[[3,8],[1,4]]]]",
        --     "[[2,[2,2]],[8,[8,1]]]",
        --     "[2,9]",
        --     "[1,[[[9,3],9],[[9,0],[0,7]]]]",
        --     "[[[5,[7,4]],7],1]",
        --     "[[[[4,2],2],6],[8,7]]"
        --   ],
        --   "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"
        -- )
        (["[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]", "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]"], "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]")
      ]
    mkCase (ds, expected) = TestCase $ assertEqual ("sum of " ++ intercalate ", " (map show ds) ++ " = " ++ show expected) expected (show (sumDigits dsParsed))
      where
        dsParsed = map (fromRight (Regular 0) . parseDigit) ds

main :: String -> IO ()
main input = do
  runTestTT tests
  putStrLn "Done."