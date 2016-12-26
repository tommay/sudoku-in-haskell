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
import Debug.Trace

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

dtrace :: Bool -> String -> a -> a
dtrace bool string result =
  if bool then trace string result else result

-- This uses the strategy of removing some cells and trying to solve
-- them back.  However, the Solver relies on having all the Unknowns
-- available, and finds the easiest Unknowns to solve.  It
-- doesn't/can't solver for particular Unknowns, at least bot without
-- even more inelegant surgery and algorithmic hoop-jumping.  Oh well.

create' :: [[Int]] -> Solver -> (Puzzle -> Solver) -> Puzzle
create' cellNumberLists solver makeSolver =
  Solver.puzzle $ foldr
    (\ list accumSolver ->
      -- We know accumSolver's puzzle has only one solution.
      -- Remove more stuff and check if that's still true.
      let newSolver = Solver.remove accumSolver list
          xSolver = makeSolver $ Solver.puzzle newSolver
          newHasOne = hasOneSolution newSolver
          xHasOne = hasOneSolution xSolver
          nextSolver = if newHasOne
            then
              -- newSolver's puzzle has only one solution, go with it.
              newSolver
            else
              -- Ooops, removed too much, stick with the original.
              accumSolver
      in trace (unlines [
           "cells: " ++ show list,
           (unwords $ ["one:", show $ newHasOne == xHasOne, show newHasOne, show xHasOne]),
           "new:",
           show newSolver,
           Puzzle.toPuzzleString $ Solver.puzzle newSolver,
           "x:",
           show xSolver,
           Puzzle.toPuzzleString $ Solver.puzzle xSolver
           ])
           nextSolver)
    solver
    cellNumberLists

hasOneSolution :: Solver -> Bool
hasOneSolution solver =
  case Solver.solve solver of
    [_] -> True
    _ -> False

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
