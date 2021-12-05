{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}

module Main where

import qualified Debug.Trace as Debug
import qualified Text.Read as Read
import qualified Data.Maybe as Maybe
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Foldable as Fold
import qualified Control.Applicative as Ap

import qualified Data.Set as Set
import Data.Set (Set)

main :: IO ()
main = do
  contents <- readFile "hydrothermal-venture.txt"
  print (hydrothermalVenture2 contents)

--------------------------
--- DAY 1: SONAR SWEEP ---
--------------------------

trim :: String -> String
trim = let
  f = reverse . dropWhile Char.isSpace
  in f . f

readTrim :: String -> Maybe Int
readTrim = Read.readMaybe . trim

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [_] = []
pairs (x:y:rest) = (x, y) : pairs (y:rest)

triples :: [a] -> [(a, a, a)]
triples [] = []
triples [_] = []
triples [_, _] = []
triples (x:y:z:rest) = (x, y, z) : triples(y:z:rest)

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

sonarSweep1 :: String -> Int
sonarSweep1 file = let
  nums = Maybe.mapMaybe readTrim (lines file)
  ordered = uncurry (<)
  in count ordered (pairs nums)

sonarSweep2 :: String -> Int
sonarSweep2 file = let
  nums = Maybe.mapMaybe readTrim (lines file)
  ordered = uncurry (<)
  sum3 (x, y, z) = x + y + z
  windows = map sum3 (triples nums)
  in count ordered (pairs windows)

-------------------
--- DAY 2: DIVE ---
-------------------

headMaybe [] = Nothing
headMaybe (x:_) = Just x

tailMaybe [] = Nothing
tailMaybe (_:xs) = Just xs

data Direction = Up | Down | Forward deriving (Eq)

readDirection :: String -> Maybe Direction
readDirection "up" = Just Up
readDirection "down" = Just Down
readDirection "forward" = Just Forward
readDirection _ = Nothing

readCommand :: String -> Maybe (Direction, Int)
readCommand line = do
  let parts = words line
  dirStr <- headMaybe parts
  numStr <- headMaybe =<< tailMaybe parts
  dir <- readDirection dirStr
  num <- Read.readMaybe numStr
  return (dir, num)

sumDirection :: Direction -> [(Direction, Int)] -> Int
sumDirection _ [] = 0
sumDirection toSum ((dir, num) : rest)
  | dir == toSum = num + sumDirection toSum rest
  | otherwise = sumDirection toSum rest

dive1 :: String -> Int
dive1 file = let
  commands = Maybe.mapMaybe readCommand (lines file)
  ups = sumDirection Up commands
  downs = sumDirection Down commands
  pos = sumDirection Forward commands
  depth = downs - ups
  in pos * depth

updatePosition :: (Int, Int, Int) -> (Direction, Int) -> (Int, Int, Int)
updatePosition (aim, pos, depth) (dir, num) =
  case dir of
    Up -> (aim - num, pos, depth)
    Down -> (aim + num, pos, depth)
    Forward -> (aim, pos + num, depth + aim * num)

dive2 :: String -> Int
dive2 file = let
  commands = Maybe.mapMaybe readCommand (lines file)
  (_, pos, depth) = foldl updatePosition (0, 0, 0) commands
  in pos * depth

--------------------------------
--- DAY 3: BINARY DIAGNOSTIC ---
--------------------------------

type BinaryRow = [Bool]

readBinaryRow :: String -> Maybe BinaryRow
readBinaryRow = let
  readBit '0' = Just False
  readBit '1' = Just True
  readBit _ = Nothing
  in mapM readBit . trim

modeBool :: [Bool] -> Bool
modeBool bs = let
  (ts, fs) = List.partition id bs
  in length ts >= length fs

columnModes :: [BinaryRow] -> BinaryRow
columnModes = map modeBool . List.transpose

rowToInt :: BinaryRow -> Int
rowToInt = let
  build _ [] = 0
  build factor (x:xs) = factor * fromEnum x + build (factor * 2) xs
  in build 1 . reverse

binaryDiagnostic1 :: String -> Int
binaryDiagnostic1 file = let
  rows = Maybe.mapMaybe readBinaryRow (lines file)
  gamma = columnModes rows
  epsilon = map not gamma
  in rowToInt gamma * rowToInt epsilon

getMaybe :: Int -> [a] -> Maybe a
getMaybe _ [] = Nothing
getMaybe 0 (x:_) = Just x
getMaybe index (x:xs) = getMaybe (index - 1) xs

matchesAt :: (Eq a) => Int -> a -> [a] -> Bool
matchesAt index test list
  | Just value <- getMaybe index list = value == test
  | otherwise = False

filterRowsBy :: (Bool -> Bool) -> Int -> [BinaryRow] -> Maybe [BinaryRow]
filterRowsBy f index rows = do
  col <- mapM (getMaybe index) rows
  let mode = f (modeBool col)
  return $ filter (matchesAt index mode) rows

extractRowBy :: (Bool -> Bool) -> Int -> [BinaryRow] -> Maybe BinaryRow
extractRowBy f index rows = do
  matching <- filterRowsBy f index rows
  case matching of
    [] -> Nothing
    [row] -> Just row
    _ -> extractRowBy f (index + 1) matching

binaryDiagnostic2 :: String -> Int
binaryDiagnostic2 file = let
  rows = Maybe.mapMaybe readBinaryRow (lines file)
  modes = columnModes rows
  extract f = Maybe.fromMaybe [] (extractRowBy f 0 rows)
  oxygen = extract id
  carbon = extract not
  in rowToInt oxygen * rowToInt carbon

--------------------------
--- DAY 4: GIANT SQUID ---
--------------------------

type BingoBoard = [[(Int, Bool)]]

spaceSepNums :: String -> [Int]
spaceSepNums = Maybe.mapMaybe Read.readMaybe . words

commaSepNums :: String -> [Int]
commaSepNums = let
  replace ',' = ' '
  replace c = c
  in spaceSepNums . map replace

validBoard :: BingoBoard -> Bool
validBoard board = let
  length5 xs = length xs == 5
  in length5 board && all length5 board

bingoBoards :: [Int] -> [BingoBoard]
bingoBoards nums = let
  split5 [] = []
  split5 xs = take 5 xs : split5 (drop 5 xs)
  tables = split5 (split5 nums)
  boards = (map . map . map) (, False) tables
  in take 100 boards

playRound :: Int -> [BingoBoard] -> [BingoBoard]
playRound num boards = let
  mark (x, _) | x == num = (x, True)
  mark square = square
  in (map . map . map) mark boards

isWinner :: BingoBoard -> Bool
isWinner board = let
  winningRow row = all snd row
  in any winningRow board || any winningRow (List.transpose board)

findWinner :: [BingoBoard] -> Maybe BingoBoard
findWinner = headMaybe . filter isWinner

scoreBingo :: Int -> BingoBoard -> Int
scoreBingo num board = let
  getUnmarked (x, False) = Just x
  getUnmarked _ = Nothing
  unmarked = concatMap (Maybe.mapMaybe getUnmarked) board
  in sum unmarked * num

playBingo :: [Int] -> [BingoBoard] -> Maybe Int
playBingo [] _ = Nothing
playBingo (num : nums) boards = let
  boards' = playRound num boards
  in case findWinner boards' of
    Nothing -> playBingo nums boards'
    Just winner -> Just (scoreBingo num winner)

giantSquid1 :: String -> Int
giantSquid1 file = let
  (headLine, tailLines) = Maybe.fromMaybe ("", []) $ List.uncons (lines file)
  nums = commaSepNums headLine
  boards = bingoBoards $ spaceSepNums (unwords tailLines)
  in Maybe.fromMaybe 0 (playBingo nums boards)

playReverseBingo :: [Int] -> [BingoBoard] -> Maybe Int
playReverseBingo [] _ = Nothing
playReverseBingo (num : nums) boards = let
  boards' = playRound num boards
  in case filter (not . isWinner) boards' of
    [] -> Nothing
    [loser] -> playBingo nums [loser]
    losers -> playReverseBingo nums losers

giantSquid2 :: String -> Int
giantSquid2 file = let
  (headLine, tailLines) = Maybe.fromMaybe ("", []) $ List.uncons (lines file)
  nums = commaSepNums headLine
  boards = bingoBoards $ spaceSepNums (unwords tailLines)
  in Maybe.fromMaybe 0 (playReverseBingo nums boards)

-----------------------------------
--- DAY 5: Hydrothermal Venture ---
-----------------------------------

data OceanLine = OceanLine (Int, Int) (Int, Int)

readOceanLine :: String -> Maybe OceanLine
readOceanLine str = let
  replace ',' = ' '
  replace '-' = ' '
  replace '>' = ' '
  replace c = c
  in case words (map replace str) of
    [s1, s2, s3, s4] -> do
      x1 <- Read.readMaybe s1
      x2 <- Read.readMaybe s2
      x3 <- Read.readMaybe s3
      x4 <- Read.readMaybe s4
      return $ OceanLine (x1, x2) (x3, x4)
    _ -> Nothing

isOrthogonal :: OceanLine -> Bool
isOrthogonal (OceanLine (x1, y1) (x2, y2)) =
  x1 == x2 || y1 == y2

toCoordinates :: OceanLine -> Set (Int, Int)
toCoordinates (OceanLine (x1, y1) (x2, y2)) = let
  xStep = signum (x2 - x1)
  yStep = signum (y2 - y1)
  xCoords = [x1, x1 + xStep .. x2]
  yCoords = [y1, y1 + yStep .. y2]
  in Set.fromList (zip xCoords yCoords)

cannotOverlap :: OceanLine -> OceanLine -> Bool
cannotOverlap (OceanLine (x1, y1) (x2, y2)) (OceanLine (x3, y3) (x4, y4)) =
  max x1 x2 < min x3 x4 || min x1 x2 > max x3 x4 ||
  max y1 y2 < min y3 y4 || min y1 y2 > max y3 y4

lineOverlap :: OceanLine -> OceanLine -> Set (Int, Int)
lineOverlap line1 line2
  | cannotOverlap line1 line2 = Set.empty
  | otherwise = Set.intersection
      (toCoordinates line1)
      (toCoordinates line2)

uniquePairs :: [a] -> [(a, a)]
uniquePairs [] = []
uniquePairs (x:xs) = zip (repeat x) xs ++ uniquePairs xs

allOverlaps :: [OceanLine] -> Set (Int, Int)
allOverlaps lns = Fold.fold do
  (line1, line2) <- uniquePairs lns
  return (lineOverlap line1 line2)

hydrothermalVenture1 :: String -> Int
hydrothermalVenture1 file = let
  oceanLines = Maybe.mapMaybe readOceanLine (lines file)
  orthogonal = filter isOrthogonal oceanLines
  overlaps = allOverlaps orthogonal
  in Set.size overlaps

hydrothermalVenture2 :: String -> Int
hydrothermalVenture2 file = let
  oceanLines = Maybe.mapMaybe readOceanLine (lines file)
  overlaps = allOverlaps oceanLines
  in Set.size overlaps
