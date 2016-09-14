import qualified Data.List as List
import qualified Data.Map as Map
import qualified System.Random as Random
import qualified System.Random.Shuffle as Shuffle

import qualified Puzzle
import Puzzle (Puzzle)

main = do
  gen <- Random.getStdGen
  putStr $ Puzzle.toPuzzleString $ create gen typicalCellSet

randomSolvedPuzzle :: Random.StdGen -> Puzzle
randomSolvedPuzzle gen =
  head $ Puzzle.randomSolutions Puzzle.empty gen

typicalCellSet :: Int -> [Int]
typicalCellSet n =
  List.nub [n, 80 - n]

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

create :: Random.StdGen -> (Int -> [Int]) -> Puzzle
create gen func =
  let puzzle = randomSolvedPuzzle gen
      sets = shuffle gen $ cellSets func
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
         create' puzzle rest
       False ->
         create' newPuzzle rest

hasMultipleSolutions :: Puzzle -> Bool
hasMultipleSolutions puzzle =
  (length $ take 2 $ Puzzle.solutions puzzle) == 2

shuffle :: Random.StdGen -> [a] -> [a]
shuffle gen list = do
  Shuffle.shuffle' list (length list) gen
