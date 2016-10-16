module Creater (
  create,
  createList
) where

import qualified Puzzle
import           Puzzle (Puzzle)
import qualified Solution
import qualified Solver
import qualified Util

import qualified System.Random as Random

create :: Random.StdGen -> [[Int]] -> Puzzle
create rnd layout =
  let (rnd1, rnd2) = Random.split rnd
      puzzle = randomSolvedPuzzle rnd1
      layout' = Util.shuffle rnd2 layout
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
      randomSolution = head $ Solver.fastRandomSolutions emptyPuzzle rnd
      randomPuzzle = Solution.puzzle randomSolution
  in randomPuzzle

hasOnlyOneSolution :: Puzzle -> Bool
hasOnlyOneSolution puzzle =
  (length $ take 2 $ Solver.fastSolutions puzzle) == 1
