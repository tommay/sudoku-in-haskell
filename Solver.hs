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
  solutions_a puzzle Nothing 0 []

randomSolutions :: Puzzle -> Random.StdGen -> [Solution]
randomSolutions puzzle rnd =
  solutions_a puzzle (Just rnd) 0 []

solutions_a :: Puzzle -> Maybe Random.StdGen -> Int -> [Solution] -> [Solution]
solutions_a puzzle maybeRnd guessCount results =
  case Puzzle.unknown puzzle of
    [] ->
      -- No more unknowns, solved!
      Solution.new guessCount puzzle : results
    _ -> solutions_b puzzle maybeRnd guessCount results

solutions_b :: Puzzle -> Maybe Random.StdGen -> Int -> [Solution] -> [Solution]
solutions_b puzzle maybeRnd guessCount results =
  if True
    then -- Try the heuristic functions.
      let maybeNext = first $
            map (\ f -> f puzzle)
              [placeOneMissing, placeOneNeeded, placeOneForced]
      in case maybeNext of
        Just nextPuzzle ->
          solutions_a nextPuzzle maybeRnd guessCount results
        Nothing ->
          solutions_c puzzle maybeRnd guessCount results
    else -- Skip the heuristics and continue with solutions_c.
      solutions_c puzzle maybeRnd guessCount results

solutions_c :: Puzzle -> Maybe Random.StdGen -> Int -> [Solution] -> [Solution]
solutions_c puzzle maybeRnd guessCount results =
  -- We get here because we can't place a digit using human-style
  -- heuristics, so we've either failed or we have to guess and
  -- recurse.  We can distinguish by examining the cell with the
  -- fewest possibilities remaining, which is also the best cell to
  -- make a guess for.
  -- XXX May be faster to find minUnknown before even trying the heuristics
  -- so we can fail faster.  We may even chug along with the heuristics
  -- for a while before realizing we made a failing guess.

  let minUnknown = minByNumPossible $ Puzzle.unknown puzzle
      possible = Unknown.possible minUnknown
  in case possible of
    [] ->
      -- Failed.  No more solutions.
      results
    [_] ->
      -- One possibility.  Recurse without incrementing guessCount.
      -- This will not happen if we're using the heuristics, but
      -- this case is included in case they're disabled.
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

-- For each digit in the list, use it as a guess for unknown
-- and try to solve the resulting Puzzle.
--
doGuesses :: Puzzle -> Maybe Random.StdGen -> Int -> Unknown -> [Int] -> [Solution] -> [Solution]
doGuesses puzzle maybeRnd guessCount unknown digits results =
  foldr (\ digit accum ->
          let guess = Puzzle.place puzzle unknown digit
          in solutions_a guess maybeRnd guessCount accum)
    results
    digits

-- Try to place a digit where a set has only one unplaced cell.
placeOneMissing :: Puzzle -> Maybe Puzzle
placeOneMissing puzzle =
  Solver.any (placeOneMissingInSet puzzle) $ Puzzle.exclusionSets puzzle

placeOneMissingInSet :: Puzzle -> [Int] -> Maybe Puzzle
placeOneMissingInSet puzzle set =
  case unknownsInSet puzzle set of
    [unknown] ->
      -- Exactly one cell in the set is unknown.  Place a digit in it.
      -- Note that since this is the only unknown position in the set
      -- there should be exactly one possible digit remaining.  But we
      -- may have made a wrong guess, which leaves no possibilities.
      case Unknown.possible unknown of
        [digit] -> Just $ Puzzle.place puzzle unknown digit
        [] -> Nothing
    _ ->
      -- Zero or multiple cells in the set are unknown.
      Nothing

unknownsInSet :: Puzzle -> [Int] -> [Unknown]
unknownsInSet puzzle set =
  filter
    (\ unknown -> (Unknown.cellNumber unknown) `elem` set)
    $ Puzzle.unknown puzzle

-- Try to place a digit where there is a set doesn't yet have some
-- digit (it needs it) and there is only one cell in the set where it
-- can possibly go, and return Just Puzzle if a digit was placed.
-- This is pretty inefficient since it has to look through all the
-- digits and cells repeatedly but so what.
--
placeOneNeeded :: Puzzle -> Maybe Puzzle
placeOneNeeded puzzle =
  Solver.any (placeOneNeededInSet puzzle) $ Puzzle.exclusionSets puzzle

placeOneNeededInSet :: Puzzle -> [Int] -> Maybe Puzzle
placeOneNeededInSet puzzle set =
  let unknowns = unknownsInSet puzzle set
  in Solver.any (placeNeededDigitInSet puzzle unknowns) [1..9]

placeNeededDigitInSet :: Puzzle -> [Unknown] -> Int -> Maybe Puzzle
placeNeededDigitInSet puzzle unknowns digit =
  case unknownsForDigit unknowns digit of
    [unknown] -> Just $ Puzzle.place puzzle unknown digit
    _ -> Nothing

placeOneForced :: Puzzle -> Maybe Puzzle
placeOneForced puzzle =
  Solver.any (placeForcedUnknown puzzle) $ Puzzle.unknown puzzle

placeForcedUnknown :: Puzzle -> Unknown -> Maybe Puzzle
placeForcedUnknown puzzle unknown =
  case Unknown.possible unknown of
    [digit] -> Just $ Puzzle.place puzzle unknown digit
    _ -> Nothing

unknownsForDigit :: [Unknown] -> Int -> [Unknown]
unknownsForDigit unknowns digit =
  filter (\ u -> digit `elem` Unknown.possible u) unknowns

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

-- Look through a list of Maybes and return the first one that isn't
-- Nothing, or Nothing if they're all Nothing.
--
first :: [Maybe a] -> Maybe a
first [] = Nothing
first ((result @ (Just _)) : _) =
  result
first (_ : tail) =
  first tail

-- Call func with each element of the list and return the first result
-- that isn't Nothing.
--
xany :: (a -> Maybe b) -> [a] -> Maybe b
xany _ [] = Nothing
xany func (head:tail) =
  case func head of
    result @ (Just _) -> result
    Nothing -> Solver.xany func tail

-- Alternative definition using fold.
--
yany :: (a -> Maybe b) -> [a] -> Maybe b
yany func list =
  foldr (\ elem accum ->
    case accum of
      Just _ -> accum
      Nothing -> func elem)
    Nothing
    list

-- Alternative definition using first.
--
any :: (a -> Maybe b) -> [a] -> Maybe b
any func list =
  first $ map func list
