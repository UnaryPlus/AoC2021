{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}

module Main where

import qualified Debug.Trace as Debug
import qualified Text.Read as Read
import qualified Data.Maybe as Maybe
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Foldable as Fold
import qualified Data.Bifunctor as Bf
import Data.Function ((&))

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Control.Lens as Lens
import Control.Lens ((%~))

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

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x

tailMaybe :: [a] -> Maybe [a]
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
getMaybe index (_:xs) = getMaybe (index - 1) xs

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

--------------------------
--- DAY 6: LANTERNFISH ---
--------------------------

composeN :: Int -> (a -> a) -> a -> a
composeN n f
  | n <= 0 = id
  | otherwise = f . composeN (n - 1) f

type FishState = (Int, Int, Int, Int, Int, Int, Int, Int, Int)

makeFishState :: [Int] -> FishState
makeFishState = let
  inc lens = lens %~ (+1)
  add n = inc case n of
    0 -> Lens._1 ; 1 -> Lens._2
    2 -> Lens._3 ; 3 -> Lens._4
    4 -> Lens._5 ; 5 -> Lens._6
    6 -> Lens._7 ; _ -> Lens._8
  in foldr add (0, 0, 0, 0, 0, 0, 0, 0, 0)

fishDay :: FishState -> FishState
fishDay (n0, n1, n2, n3, n4, n5, n6, n7, n8) =
  (n1, n2, n3, n4, n5, n6, n7 + n0, n8, n0)

numFish :: FishState -> Int
numFish (n0, n1, n2, n3, n4, n5, n6, n7, n8) =
  n0 + n1 + n2 + n3 + n4 + n5 + n6 + n7 + n8

lanternfish1 :: String -> Int
lanternfish1 file = let
  initial = makeFishState (commaSepNums file)
  in numFish (composeN 80 fishDay initial)

lanternfish2 :: String -> Int
lanternfish2 file = let
  initial = makeFishState (commaSepNums file)
  in numFish (composeN 256 fishDay initial)

--------------------------------------
--- DAY 7: THE TREACHERY OF WHALES ---
--------------------------------------

medianInt :: [Int] -> Int
medianInt xs = let
  halfLength = length xs `div` 2
  secondHalf = drop halfLength (List.sort xs)
  in Maybe.fromMaybe 0 (headMaybe secondHalf)

differences :: Int -> [Int] -> [Int]
differences median = map (abs . subtract median)

treacheryOfWhales1 :: String -> Int
treacheryOfWhales1 file = let
  positions = commaSepNums file
  median = medianInt positions
  in sum (differences median positions)

meanInt :: [Int] -> Int
meanInt xs = let
  float = fromIntegral :: Int -> Double
  in round $ float (sum xs) / float (length xs)

triangular :: Int -> Int
triangular n = n * (n + 1) `div` 2

treacheryOfWhales2 :: String -> Int
treacheryOfWhales2 file = let
  positions = commaSepNums file
  mean = meanInt positions
  in sum $ map triangular (differences mean positions)

-----------------------------------
--- DAY 8: SEVEN SEGMENT SEARCH ---
-----------------------------------

outputValues :: String -> [String]
outputValues = drop 11 . words

uniqueLength :: String -> Bool
uniqueLength str =
  case length str of
    2 -> True ; 3 -> True
    4 -> True ; 7 -> True
    _ -> False

sevenSegmentSearch1 :: String -> Int
sevenSegmentSearch1 file = let
  outputs = concatMap outputValues (lines file)
  in count uniqueLength outputs

digitSignals :: String -> [String]
digitSignals = take 10 . words

filterLength :: Int -> [String] -> [String]
filterLength n = filter \str -> length str == n

filterCount :: Int -> String -> String
filterCount n str = filter (\c -> count (== c) str == n) str

remove :: (Eq a) => [a] -> [a] -> [a]
remove xs = filter (`notElem` xs)

signalA :: [String] -> Maybe Char
signalA strs = do
  one <- headMaybe (filterLength 2 strs)
  seven <- headMaybe (filterLength 3 strs)
  headMaybe $ filter (`notElem` one) seven

signalBDE :: [String] -> Maybe (Char, Char, Char)
signalBDE strs = do
  four <- headMaybe (filterLength 4 strs)
  let twoThreeFive = filterLength 5 strs
  let sigs = concat (four : twoThreeFive)
  sigB <- headMaybe (filterCount 2 sigs)
  sigD <- headMaybe (filterCount 4 sigs)
  sigE <- headMaybe (filterCount 1 sigs)
  return (sigB, sigD, sigE)

signalC :: (Char, Char, Char) -> [String] -> Maybe Char
signalC (_, sigD, sigE) strs = do
  let zeroSixNine = filterLength 6 strs
  let sigs = concat zeroSixNine
  let twice = filterCount 2 sigs
  headMaybe (remove [sigD, sigE] twice)

signalG :: Char -> (Char, Char, Char) -> [String] -> Maybe Char
signalG sigA (_, sigD, _) strs = do
  let twoThreeFive = filterLength 5 strs
  let sigs = concat twoThreeFive
  let thrice = filterCount 3 sigs
  headMaybe (remove [sigA, sigD] thrice)

signalF :: Char -> (Char, Char, Char) -> Char -> Char -> [String] -> Maybe Char
signalF sigA (sigB, sigD, sigE) sigC sigG strs =
  headMaybe $ remove [sigA, sigB, sigC, sigD, sigE, sigG] (concat strs)

solveSignals :: [String] -> Maybe (Char, Char, Char, Char, Char, Char, Char)
solveSignals strs = do
  sigA <- signalA strs
  sigBDE@(sigB, sigD, sigE) <- signalBDE strs
  sigC <- signalC sigBDE strs
  sigG <- signalG sigA sigBDE strs
  sigF <- signalF sigA sigBDE sigC sigG strs
  return (sigA, sigB, sigC, sigD, sigE, sigF, sigG)

data Segment = A | B | C | D | E | F | G
  deriving (Show, Eq, Ord)

makeSegment :: (Char, Char, Char, Char, Char, Char, Char) -> Char -> Maybe Segment
makeSegment (sigA, sigB, sigC, sigD, sigE, sigF, sigG) sig
  | sig == sigA = Just A
  | sig == sigB = Just B
  | sig == sigC = Just C
  | sig == sigD = Just D
  | sig == sigE = Just E
  | sig == sigF = Just F
  | sig == sigG = Just G
  | otherwise = Nothing

display :: [Segment] -> Maybe Int
display segs =
  case List.sort segs of
    [A, B, C, E, F, G] -> Just 0
    [C, F] -> Just 1
    [A, C, D, E, G] -> Just 2
    [A, C, D, F, G] -> Just 3
    [B, C, D, F] -> Just 4
    [A, B, D, F, G] -> Just 5
    [A, B, D, E, F, G] -> Just 6
    [A, C, F] -> Just 7
    [A, B, C, D, E, F, G] -> Just 8
    [A, B, C, D, F, G] -> Just 9
    _ -> Nothing

displayString :: (Char, Char, Char, Char, Char, Char, Char) -> String -> Maybe Int
displayString sigs str = do
  segments <- mapM (makeSegment sigs) str
  display segments

solveLine :: String -> Maybe [Int]
solveLine line = do
  let sigs = digitSignals line
  solution <- solveSignals sigs
  let outputs = outputValues line
  mapM (displayString solution) outputs

digitsToInt :: [Int] -> Int
digitsToInt = let
  build _ [] = 0
  build factor (x:xs) = factor * x + build (factor * 10) xs
  in build 1 . reverse

sevenSegmentSearch2 :: String -> Int
sevenSegmentSearch2 file = let
  digits = Maybe.mapMaybe solveLine (lines file)
  ints = map digitsToInt digits
  in sum ints

--------------------------
--- DAY 9: SMOKE BASIN ---
--------------------------

type Altitudes = [[Int]]

charToInt :: Char -> Maybe Int
charToInt c = Read.readMaybe [c]

readAltitudes :: [String] -> Maybe Altitudes
readAltitudes = let
  toInt c = Read.readMaybe [c]
  in mapM (mapM toInt)

getAltitude :: Altitudes -> (Int, Int) -> Maybe Int
getAltitude alts (r, c) = do
  row <- getMaybe r alts
  getMaybe c row

getLowPoint :: Altitudes -> (Int, Int) -> Maybe Int
getLowPoint alts (r, c) = let
  neighbors = Maybe.mapMaybe (getAltitude alts)
    [ (r - 1, c), (r + 1, c)
    , (r, c - 1), (r, c + 1)
    ]
  in do
  alt <- getAltitude alts (r, c)
  if all (> alt) neighbors
    then Just alt
    else Nothing

lowPoints :: Altitudes -> [Int]
lowPoints alts = let
  rows = length alts
  cols = maybe 0 length (getMaybe 0 alts)
  coords = (,) <$> [0 .. rows - 1] <*> [0 .. cols - 1]
  in Maybe.mapMaybe (getLowPoint alts) coords

smokeBasin1 :: String -> Int
smokeBasin1 file = let
  altitudes = readAltitudes $ map trim (lines file)
  in maybe 0 (sum . map (1+) . lowPoints) altitudes

indexNines :: [Int] -> [Int]
indexNines = let
  add (i, 9) indexes = i : indexes
  add _ indexes = indexes
  in foldr add [] . zip [0..]

type Gully = (Int, Int, Int)
type Basin = [Gully]

gulliesRow :: Int -> [Int] -> [Gully]
gulliesRow num row = let
  nines = -1 : indexNines row ++ [length row]
  toGully (start, end)
    | start + 1 == end = Nothing
    | otherwise = Just (num, start + 1, end)
  in Maybe.mapMaybe toGully (pairs nines)

gullies :: Altitudes -> [Gully]
gullies alts = Fold.fold (zipWith gulliesRow [0..] alts)

connected :: Gully -> Gully -> Bool
connected (num1, start1, end1) (num2, start2, end2)
  | abs (num1 - num2) /= 1 = False
  | start2 >= end1 = False
  | start1 >= end2 = False
  | otherwise = True

connectedBasin :: Basin -> Gully -> Bool
connectedBasin basin gully = any (connected gully) basin

extendBasin :: (Basin, [Gully]) -> (Basin, [Gully])
extendBasin ([], guls) = ([], guls)
extendBasin (basin, guls) =
  List.partition (connectedBasin basin) guls
  & extendBasin
  & Bf.first (basin ++)

makeBasin :: [Gully] -> (Basin, [Gully])
makeBasin [] = ([], [])
makeBasin (gul : guls) = extendBasin ([gul], guls)

makeBasins :: [Gully] -> [Basin]
makeBasins guls = let
  (basin, guls') = makeBasin guls
  in if null guls'
    then [basin]
    else basin : makeBasins guls'

basinSize :: Basin -> Int
basinSize = let
  gullySize (_, start, end) = end - start
  in sum . map gullySize

smokeBasin2 :: String -> Int
smokeBasin2 file = let
  altitudes = readAltitudes $ map trim (lines file)
  guls = maybe [] gullies altitudes
  basins = makeBasins guls
  sizes = Debug.traceShow (take 10 guls) map basinSize basins
  in product $ take 3 $ reverse (List.sort sizes)

------------------------------
--- DAY 10: SYNTAX SCORING ---
------------------------------

data Shape = Round | Square | Curly | Angle
  deriving (Show, Eq)

data Chiralty = Opening | Closing
  deriving (Show)

type Bracket = (Shape, Chiralty)

readBracket :: Char -> Maybe Bracket
readBracket = \case
  '(' -> Just (Round, Opening)
  '[' -> Just (Square, Opening)
  '{' -> Just (Curly, Opening)
  '<' -> Just (Angle, Opening)
  ')' -> Just (Round, Closing)
  ']' -> Just (Square, Closing)
  '}' -> Just (Curly, Closing)
  '>' -> Just (Angle, Closing)
  _ -> Nothing

parseBracket :: [Shape] -> Bracket -> Either Shape [Shape]
parseBracket expected (shape, Opening) =
  Right (shape : expected)
parseBracket expected (shape, Closing) =
  case expected of
    s : rest | s == shape -> Right rest
    _ -> Left shape

parseLine :: String -> Either Shape [Shape]
parseLine line = let
  brackets = Maybe.mapMaybe readBracket line
  in Fold.foldlM parseBracket [] brackets

bracketScore :: Shape -> Int
bracketScore = \case
  Round -> 3
  Square -> 57
  Curly -> 1197
  Angle -> 25137

scoreLine :: String -> Int
scoreLine line =
  case parseLine line of
    Left shape -> bracketScore shape
    Right _ -> 0

syntaxScoring1 :: String -> Int
syntaxScoring1 = sum . map scoreLine . lines

completeLine :: String -> Maybe [Shape]
completeLine line =
  case parseLine line of
    Left _ -> Nothing
    Right expected -> Just expected

scoreCompletion :: [Shape] -> Int
scoreCompletion = let
  add score shape =
    score * 5 +
    case shape of
      Round -> 1; Square -> 2
      Curly -> 3; Angle -> 4
  in foldl add 0

syntaxScoring2 :: String -> Int
syntaxScoring2 file = let
  completions = Maybe.mapMaybe completeLine (lines file)
  scores = map scoreCompletion completions
  in medianInt scores

-----------------------------
--- DAY 11: DUMBO OCTOPUS ---
-----------------------------

type Octopus = (Int, Bool)
type OctopusGrid = [[Octopus]]

readGrid :: [String] -> Maybe OctopusGrid
readGrid = let
  toOctopus c = (, False) <$> Read.readMaybe [c]
  in mapM (mapM toOctopus)

increaseEnergy :: OctopusGrid -> OctopusGrid
increaseEnergy = (map . map . Bf.first) (1+)

getOctopus :: (Int, Int) -> OctopusGrid -> Maybe Octopus
getOctopus (r, c) octs = do
  row <- getMaybe r octs
  getMaybe c row

updateList :: Int -> (a -> a) -> [a] -> [a]
updateList index f xs =
  case getMaybe index xs of
    Nothing -> xs
    Just x -> take index xs ++ [f x] ++ drop (index + 1) xs

updateOctopus :: (Int, Int) -> (Octopus -> Octopus) -> OctopusGrid -> OctopusGrid
updateOctopus (r, c) f octs = let
  updateRow = updateList c f
  in updateList r updateRow octs

flashOctopus :: (Int, Int) -> OctopusGrid -> OctopusGrid
flashOctopus (r, c) octs  =
  case getOctopus (r, c) octs  of
    Nothing -> octs
    Just (_, True) -> octs
    Just (energy, _) | energy < 10 -> octs
    Just _ -> let
      flashed = updateOctopus (r, c) (Bf.second (const True)) octs
      increase (r', c') = updateOctopus (r', c') (Bf.first (1+))
      increaseAll = foldr (.) id . map increase
      in increaseAll
        [ (r - 1, c), (r + 1, c)
        , (r, c - 1), (r, c + 1)
        , (r - 1, c - 1), (r + 1, c - 1)
        , (r - 1, c + 1), (r + 1, c + 1)
        ] flashed

flashGrid :: OctopusGrid -> OctopusGrid
flashGrid octs = let
  rows = length octs
  cols = maybe 0 length (getMaybe 0 octs)
  coords = (,) <$> [0 .. rows - 1] <*> [0 .. cols - 1]
  flashed = foldr flashOctopus octs coords
  in if flashed == octs
    then flashed
    else flashGrid flashed

renewGrid :: OctopusGrid -> (Int, OctopusGrid)
renewGrid octs = let
  didFlash (_, b) = b
  renew (energy, False) = (energy, False)
  renew (_, True) = (0, False)
  in (sum $ map (count didFlash) octs, map (map renew) octs)

stepGrid :: OctopusGrid -> (Int, OctopusGrid)
stepGrid = renewGrid . flashGrid . increaseEnergy

composeSum :: (b -> (Int, c)) -> (a -> (Int, b)) -> a -> (Int, c)
composeSum f g x = let
  (num1, x1) = g x
  (num2, x2) = f x1
  in (num1 + num2, x2)

composeSumN :: Int -> (a -> (Int, a)) -> a -> (Int, a)
composeSumN n f
  | n <= 0 = (0, )
  | otherwise = composeSum f $ composeSumN (n - 1) f

dumboOctopus1 :: String -> Int
dumboOctopus1 file =
  case readGrid (lines file) of
    Nothing -> 0
    Just octs -> fst (composeSumN 100 stepGrid octs)

stepAllFlash :: Int -> OctopusGrid -> Int
stepAllFlash step octs = let
  (num, octs') = stepGrid octs
  in if num == sum (map length octs)
    then step
    else stepAllFlash (step + 1) octs'

dumboOctopus2 :: String -> Int
dumboOctopus2 file =
  case readGrid (lines file) of
    Nothing -> 0
    Just octs -> stepAllFlash 1 octs

---------------------
--- MAIN FUNCTION ---
---------------------

main :: IO ()
main = do
  file <- readFile "input/dumbo-octopus.txt"
  print (dumboOctopus2 file)
