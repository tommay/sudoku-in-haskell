module Creater (
  create,
  createList
) where

import qualified Solver
import qualified Solution
import qualified Puzzle
import Puzzle (Puzzle)

import qualified System.Random as Random
import qualified System.Random.Shuffle as Shuffle

create :: Random.StdGen -> [[Int]] -> Puzzle
create rnd layout =
  let puzzle = randomSolvedPuzzle rnd
      layout' = shuffle rnd layout
  in create' puzzle layout'

create' :: Puzzle -> [[Int]] -> Puzzle
create' puzzle cellNumberLists =
  foldr
    (\ list accum ->
      -- We know accum has only one solution.
      -- Remove more stuff and check if that's still true.
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

createList :: Random.StdGen -> [[Int]] -> [Puzzle]
createList rnd layout =
  let (rnd1, rnd2) = Random.split rnd
      puzzle = create rnd1 layout
  in puzzle : (createList rnd2 layout)

randomSolvedPuzzle :: Random.StdGen -> Puzzle
randomSolvedPuzzle rnd =
  let emptyPuzzle = Puzzle.empty
      randomSolution = head $ Solver.randomSolutions emptyPuzzle rnd
      randomPuzzle = Solution.puzzle randomSolution
  in randomPuzzle

hasOnlyOneSolution :: Puzzle -> Bool
hasOnlyOneSolution puzzle =
  (length $ take 2 $ Solver.solutions puzzle) == 1

shuffle :: Random.StdGen -> [a] -> [a]
shuffle rnd list =
  Shuffle.shuffle' list (length list) rnd
