import qualified Data.List as List
import qualified Data.Map as Map
import qualified System.Random as Random
import qualified System.Random.Shuffle as Shuffle

import qualified Puzzle
import Puzzle (Puzzle)

cellSet = doubleDiagonalSet

main = mainOneNoGuessing

mainOne = do
  rnd <- Random.getStdGen
  putStr $ Puzzle.toPuzzleString $ create rnd cellSet

mainOneNoGuessing = do
  rnd <- Random.getStdGen
  putStr $ Puzzle.toPuzzleString $ createNoGuessing rnd cellSet

mainStreamNoGuessing = do
  rnd <- Random.getStdGen
  mapM_ putStrLn $ map
    (\ (guesses, puzzle) ->
      unlines ["Guesses: " ++ (show guesses),
               Puzzle.toPuzzleString puzzle])
    $ filter (\ (g, p) -> g == 0) $ create'' rnd cellSet

classicCellSet :: Int -> [Int]
classicCellSet n =
  List.nub [n, 80 - n]

reflectLeftRight :: Int -> Int
reflectLeftRight n =
  let col = n `mod` 9
  in n - col + 8 - col

leftRightCellSet :: Int -> [Int]
leftRightCellSet n =
  List.nub [n, reflectLeftRight n]

reflectUpDown :: Int -> Int
reflectUpDown n =
  let rowish = (n `div` 9) * 9
  in n - rowish + 72 - rowish

leftRightUpDownCellSet :: Int -> [Int]
leftRightUpDownCellSet n =
  let leftRight = leftRightCellSet n
  in List.nub $ leftRight ++ map reflectUpDown leftRight

identicalCellSet :: Int -> [Int]
identicalCellSet n =
  let col = n `mod` 3
      row = (n `div` 9) `mod` 3
      base = row*9 + col
  in List.nub $ map (base+) [0, 3, 6, 27, 30, 33, 54, 57, 60]

spinnySets :: [[Int]]
spinnySets = [
  [0, 8, 80, 72],
  [1, 17, 79, 63],
  [2, 26, 78, 54],
  [3, 35, 77, 45],
  [4, 44, 76, 36],
  [5, 53, 75, 27],
  [6, 62, 74, 18],
  [7, 71, 73, 9],
  [10, 16, 70, 64],
  [11, 25, 69, 55],
  [12, 34, 68, 46],
  [13, 43, 67, 37],
  [14, 52, 66, 28],
  [15, 61, 65, 19],
  [20, 24, 60, 56],
  [21, 33, 59, 47],
  [22, 42, 58, 38],
  [23, 51, 57, 29],
  [30, 32, 50, 48],
  [31, 41, 49, 39],
  [40]]

randomCellSet :: Int -> [Int]
randomCellSet n = [n]

spinnyCellSet :: Int -> [Int]
spinnyCellSet n =
  case List.find (\set -> n `elem` set) spinnySets of
    Just set -> set

reflectDiagonally :: Int -> Int
reflectDiagonally n =
  let (row, col) = rowcol n
  in col*9 + row

diagonalSet :: Int -> [Int]
diagonalSet n = 
  List.nub [n, reflectDiagonally n]

reflectDiagonally' :: Int -> Int
reflectDiagonally' n =
  let (row, col) = rowcol n
      col' = 8 - col
      row' = 8 - row
  in col'*9 + row'

diagonal'Set :: Int -> [Int]
diagonal'Set n = 
  List.nub [n, reflectDiagonally' n]

doubleDiagonalSet :: Int -> [Int]
doubleDiagonalSet n =
  let diagonal = diagonalSet n
  in List.nub $ diagonal ++ map reflectDiagonally' diagonal

wtf2 :: Int -> Int
wtf2 n =
  let (row, col) = rowcol (80 - n)
  in col*9 + row

wtf2Set :: Int -> [Int]
wtf2Set n = 
  List.nub [n, wtf2 n]

wtfSet :: Int -> [Int]
wtfSet n = 
  let col = 8 - (n `mod` 9)
      row = n `div` 9
  in List.nub [n, col*9 + row]

rowcol :: Int -> (Int, Int)
rowcol n =
  (n `div` 9, n `mod` 9)

randomSolvedPuzzle :: Random.StdGen -> Puzzle
randomSolvedPuzzle rnd =
  let (_, puzzle) = head $ Puzzle.randomSolutions Puzzle.empty rnd
  in puzzle

cellSets :: (Int -> [Int]) -> [[Int]]
cellSets func =
  uniqueCellSets $ map func [0..80]

uniqueCellSets :: [[Int]] -> [[Int]]
uniqueCellSets cellSets =
  let uniqueMap =
        foldr
          (\ cellSet map -> Map.insert (minimum cellSet) cellSet map)
          Map.empty
          cellSets
  in Map.elems uniqueMap

createNoGuessing :: Random.StdGen -> (Int -> [Int]) -> Puzzle
createNoGuessing rnd func =
  head $ take 1 $ map snd $ filter (\ (g, p) -> g == 0) $ create'' rnd func

create'' :: Random.StdGen -> (Int -> [Int]) -> [(Int, Puzzle)]
create'' rnd func =
  let (rnd1, rnd2) = Random.split rnd
      puzzle = create rnd1 func
      (guesses, _) = head $ Puzzle.solutions puzzle
  in (guesses, puzzle):(create'' rnd2 func)

create :: Random.StdGen -> (Int -> [Int]) -> Puzzle
create rnd func =
  let puzzle = randomSolvedPuzzle rnd
      sets = shuffle rnd $ cellSets func
  in create' puzzle sets

create' :: Puzzle -> [[Int]] -> Puzzle
create' puzzle [] =
  puzzle
create' puzzle cellNumberLists =
  -- We know the puzzle we're given has only one solution.
  -- Remove more stuff from Puzzle and recurse.
  let (cellNumbers:rest) = cellNumberLists
      newPuzzle = Puzzle.remove puzzle cellNumbers
  in case hasMultipleSolutions newPuzzle of
       True ->
         -- Ooops, removed too much.  Recurse with the original
         -- single-solution puzzle.
         create' puzzle rest
       False ->
         -- newPuzzle has only one solution, go with it.
         create' newPuzzle rest

hasMultipleSolutions :: Puzzle -> Bool
hasMultipleSolutions puzzle =
  (length $ take 2 $ Puzzle.solutions puzzle) == 2

shuffle :: Random.StdGen -> [a] -> [a]
shuffle rnd list = do
  Shuffle.shuffle' list (length list) rnd
