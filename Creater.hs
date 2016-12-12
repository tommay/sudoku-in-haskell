module Creater (
  create,
  createList
) where

import qualified Puzzle
import           Puzzle (Puzzle)
import qualified Solution
import           Solution (Solution)
import qualified Solver
import qualified Util

import qualified System.Random as Random

-- Returns (puzzle, solvedPuzzle)
--
createWithSolution :: Random.StdGen -> [[Int]] -> (Puzzle -> [Solution]) ->
  (Puzzle, Puzzle)
createWithSolution rnd layout solver =
  let (rnd1, rnd2) = Random.split rnd
      solvedPuzzle = randomSolvedPuzzle rnd1
      layout' = Util.shuffle rnd2 layout
      puzzle = create' solvedPuzzle layout' solver
  in (puzzle, solvedPuzzle)

create :: Random.StdGen -> [[Int]] -> (Puzzle -> [Solution]) -> Puzzle
create rnd layout solver =
  fst $ createWithSolution rnd layout solver

create' :: Puzzle -> [[Int]] -> (Puzzle -> [Solution])  -> Puzzle
create' puzzle cellNumberLists solver =
  foldr
    (\ list accum ->
      -- We know accum has only one solution.
      -- Remove more stuff and check if that's still true.
      let newPuzzle = Puzzle.remove accum list
      in case solver newPuzzle of
        [_] ->
          -- newPuzzle has only one solution, go with it.
          newPuzzle
        _ ->
          -- Ooops, removed too much, stick with the original.
          accum)
    puzzle
    cellNumberLists

createList :: Random.StdGen -> [[Int]] -> (Puzzle -> [Solution]) -> [Puzzle]
createList rnd layout solvable =
  let (rnd1, rnd2) = Random.split rnd
      puzzle = create rnd1 layout solvable
  in puzzle : (createList rnd2 layout solvable)

randomSolvedPuzzle :: Random.StdGen -> Puzzle
randomSolvedPuzzle rnd =
  let emptyPuzzle = Puzzle.empty
      randomSolution = head $ Solver.allRandomSolutions rnd emptyPuzzle
      randomPuzzle = Solution.puzzle randomSolution
  in randomPuzzle
