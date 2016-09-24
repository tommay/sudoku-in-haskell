module Solver (
  Solver.solutions,
  Solver.randomSolutions,
) where

import qualified Puzzle
import Puzzle (Puzzle)
import qualified Unknown
import Unknown (Unknown)
import qualified Solution
import Solution (Solution)

import qualified System.Random as Random
import qualified System.Random.Shuffle as Shuffle

-- Try to solve this Puzzle, returning a list of solved Puzzles.
--
solutions :: Puzzle -> [Solution]
solutions puzzle =
  solutions' puzzle Nothing 0 []

randomSolutions :: Puzzle -> Random.StdGen -> [Solution]
randomSolutions puzzle rnd =
  solutions' puzzle (Just rnd) 0 []

-- Try to solve the given Puzzle, returning a list of solved Puzzles.
--
solutions' :: Puzzle -> Maybe Random.StdGen -> Int -> [Solution] -> [Solution]
solutions' puzzle maybeRnd guessCount results =
  case Puzzle.unknown puzzle of
    [] ->
      -- No more unknowns, solved!
      Solution.new guessCount puzzle : results
    unknownCells ->
      let minUnknown = minByNumPossible unknownCells
          possible = Unknown.possible minUnknown
      in case possible of
        [] ->
          -- Failed.  No more solutions.
          results
        [_] ->
          -- One possibility.  Recurse without incrementing guessCount.
          doGuesses puzzle maybeRnd guessCount minUnknown possible results
        _ ->
          -- Multiple possibilities.  Guess each, maybe in a random order,
          -- and recurse.
          let shuffledPossible =
                case maybeRnd of
                  Nothing -> possible
                  Just rnd -> shuffle rnd possible
          in doGuesses puzzle maybeRnd
               (guessCount + 1) minUnknown shuffledPossible results

-- For each Digit in the list, use it as a guess for unknown
-- and try to solve the resulting Puzzle.
--
doGuesses :: Puzzle -> Maybe Random.StdGen -> Int -> Unknown -> [Int] -> [Solution] -> [Solution]
doGuesses puzzle maybeRnd guessCount unknown digits results =
  foldr (\ digit accum ->
          let guess = Puzzle.place puzzle unknown digit
          in solutions' guess maybeRnd guessCount accum)
    results
    digits

minByNumPossible :: [Unknown] -> Unknown
minByNumPossible  =
  minBy (length . Unknown.possible)

minBy :: Ord b => (a -> b) -> [a] -> a
minBy func list =
  let enhanced = map (\ a -> (func a, a)) list
  -- foldl1 is a smidge faster than foldr1.
  in snd $ foldl1 (\ a@(na, _) b@(nb, _) ->
       -- The results are of course the same we select a or b when
       -- na == nb, but testing na <= nb makes things much slower,
       -- probably because it chooses elements deeper in the list
       -- which makes for more list manipulation.
       if na < nb
         then a
         else b)
       enhanced

shuffle :: Random.StdGen -> [a] -> [a]
shuffle rnd list =
  Shuffle.shuffle' list (length list) rnd
