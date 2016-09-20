import qualified Data.List as List
import qualified Data.Map as Map
import qualified System.Random as Random
import qualified System.Random.Shuffle as Shuffle

import qualified Layout
import qualified Puzzle
import Puzzle (Puzzle)

cellSet = Layout.spinny

main = mainOneNoGuessing

mainOne = do
  rnd <- Random.getStdGen
  putStr $ Puzzle.toPuzzleString $ create rnd cellSet

mainOneNoGuessing = do
  rnd <- Random.getStdGen
  putStr $ Puzzle.toPuzzleString $ createNoGuessing rnd cellSet

mainListNoGuessing = do
  rnd <- Random.getStdGen
  mapM_ putStrLn $ map
    (\ (guesses, puzzle) ->
      unlines ["Guesses: " ++ (show guesses),
               Puzzle.toPuzzleString puzzle])
    $ filter (\ (g, p) -> g == 0) $ createList rnd cellSet

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
  head $ take 1 $ map snd $ filter (\ (g, p) -> g == 0) $ createList rnd func

createList :: Random.StdGen -> (Int -> [Int]) -> [(Int, Puzzle)]
createList rnd func =
  let (rnd1, rnd2) = Random.split rnd
      puzzle = create rnd1 func
      (guesses, _) = head $ Puzzle.solutions puzzle
  in (guesses, puzzle):(createList rnd2 func)

create :: Random.StdGen -> (Int -> [Int]) -> Puzzle
create rnd func =
  let puzzle = randomSolvedPuzzle rnd
      sets = shuffle rnd $ cellSets func
  in create' puzzle sets

create' :: Puzzle -> [[Int]] -> Puzzle
create' puzzle cellNumberLists =
  foldr
    (\ list accum ->
      -- We know accum has only one solution.
      -- Remove more stuff an check if that's still true.
      let newPuzzle = Puzzle.remove accum list
      in case hasOnlyOneSolution newPuzzle of
        True ->
           -- newPuzzle has only one solution, go with it.
          newPuzzle
        False ->
          -- Ooops, removed too much, stick with the original.
          accum)
    puzzle
    cellNumberLists

hasOnlyOneSolution :: Puzzle -> Bool
hasOnlyOneSolution puzzle =
  (length $ take 2 $ Puzzle.solutions puzzle) == 1

shuffle :: Random.StdGen -> [a] -> [a]
shuffle rnd list =
  Shuffle.shuffle' list (length list) rnd
