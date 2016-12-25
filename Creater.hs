module Creater (
  create,
  createList
) where

import qualified Puzzle
import           Puzzle (Puzzle)
import qualified Solution
import           Solution (Solution)
import qualified Solver
import           Solver (Solver)
import qualified Util

import qualified System.Random as Random

-- Returns (puzzle, solvedPuzzle)
--
createWithSolution :: Random.StdGen -> [[Int]] -> (Puzzle -> Solver) ->
  (Puzzle, Puzzle)
createWithSolution rnd layout makeSolver =
  let (rnd1, rnd2) = Random.split rnd
      solvedPuzzle = randomSolvedPuzzle rnd1
      solver = makeSolver solvedPuzzle
      layout' = Util.shuffle rnd2 layout
      puzzle = create' layout' solver makeSolver
  in (puzzle, solvedPuzzle)

create :: Random.StdGen -> [[Int]] -> (Puzzle -> Solver) -> Puzzle
create rnd layout makeSolver =
  fst $ createWithSolution rnd layout makeSolver

create' :: [[Int]] -> Solver -> (Puzzle -> Solver) -> Puzzle
create' cellNumberLists solver makeSolver =
  Solver.puzzle $ foldr
    (\ list accumSolver ->
      -- We know accumSolver's puzzle has only one solution.
      -- Remove more stuff and check if that's still true.
      let newSolver = Solver.remove accumSolver list
          xSolver = makeSolver $ Solver.puzzle newSolver
      in case Solver.solve xSolver of
        [_] ->
          -- newSolver's puzzle has only one solution, go with it.
          newSolver
        _ ->
          -- Ooops, removed too much, stick with the original.
          accumSolver)
    solver
    cellNumberLists

createList :: Random.StdGen -> [[Int]] -> (Puzzle -> Solver) -> [Puzzle]
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
