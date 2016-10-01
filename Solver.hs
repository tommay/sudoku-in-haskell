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
import qualified ExclusionSets
import qualified TrickySets
import TrickySets (TrickySet)

import qualified System.Random as Random
import qualified System.Random.Shuffle as Shuffle
import Control.Applicative
import Debug.Trace

tryHeuristics = False
tryTricky = False

doDebug = False

debug :: String -> b -> b
debug a b =
  if doDebug
    then trace a b
    else b

-- Try to solve this Puzzle, returning a list of solved Puzzles.
--
solutions :: Puzzle -> [Solution]
solutions puzzle =
  solutionsTop puzzle Nothing 0 []

randomSolutions :: Puzzle -> Random.StdGen -> [Solution]
randomSolutions puzzle rnd =
  solutionsTop puzzle (Just rnd) 0 []

solutionsTop :: Puzzle -> Maybe Random.StdGen -> Int -> [Solution] -> [Solution]
solutionsTop puzzle maybeRnd guessCount results =
  case Puzzle.unknown puzzle of
    [] ->
      -- No more unknowns, solved!
      Solution.new guessCount puzzle : results
    _ -> solutionsHeuristic puzzle maybeRnd guessCount results

solutionsHeuristic :: Puzzle -> Maybe Random.StdGen -> Int -> [Solution] -> [Solution]
solutionsHeuristic puzzle maybeRnd guessCount results =
  if tryHeuristics
    then -- Try the heuristic functions.
      let maybeNext = Solver.firstMaybe
            $ [placeOneMissing, placeOneNeeded, placeOneForced]
                <*> [puzzle]
      in case maybeNext of
        Just nextPuzzle ->
          solutionsTop nextPuzzle maybeRnd guessCount results
        Nothing ->
          solutionsTricky puzzle maybeRnd guessCount results
    else -- Skip the heuristics and continue with solutionsTricky.
      solutionsTricky puzzle maybeRnd guessCount results

solutionsGuess :: Puzzle -> Maybe Random.StdGen -> Int -> [Solution] -> [Solution]
solutionsGuess puzzle maybeRnd guessCount results =
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
          in solutionsTop guess maybeRnd guessCount accum)
    results
    digits

-- Try to place a digit where a set has only one unplaced cell.
placeOneMissing :: Puzzle -> Maybe Puzzle
placeOneMissing puzzle =
  Solver.any (placeOneMissingInSet puzzle) ExclusionSets.exclusionSets

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
  filter (isUnknownInSet set) $ Puzzle.unknown puzzle

-- Try to place a digit where there is a set doesn't yet have some
-- digit (it needs it) and there is only one cell in the set where it
-- can possibly go, and return Just Puzzle if a digit was placed.
-- This is pretty inefficient since it has to look through all the
-- digits and cells repeatedly but so what.
--
placeOneNeeded :: Puzzle -> Maybe Puzzle
placeOneNeeded puzzle =
  Solver.any (placeOneNeededInSet puzzle) ExclusionSets.exclusionSets

placeOneNeededInSet :: Puzzle -> [Int] -> Maybe Puzzle
placeOneNeededInSet puzzle set =
  let unknowns = unknownsInSet puzzle set
  in Solver.any (placeNeededDigitInSet puzzle unknowns) [1..9]

placeNeededDigitInSet :: Puzzle -> [Unknown] -> Int -> Maybe Puzzle
placeNeededDigitInSet puzzle unknowns digit =
  case filter (isDigitPossibleForUnknown digit) unknowns of
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

solutionsTricky :: Puzzle -> Maybe Random.StdGen -> Int -> [Solution] -> [Solution]
solutionsTricky puzzle maybeRnd guessCount results =
  if tryTricky
    then
      let maybePuzzle = Solver.any (tryTrickySet puzzle) TrickySets.trickySets
      in case maybePuzzle of
        Just newPuzzle -> solutionsTop newPuzzle maybeRnd guessCount results
        Nothing -> solutionsGuess puzzle maybeRnd guessCount results
    else
      solutionsGuess puzzle maybeRnd guessCount results

tryTrickySet :: Puzzle -> TrickySet -> Maybe Puzzle
tryTrickySet puzzle trickySet =
  Solver.any (tryTrickySetWithDigit puzzle trickySet) [1..9]

tryTrickySetWithDigit :: Puzzle -> TrickySet -> Int -> Maybe Puzzle
tryTrickySetWithDigit puzzle trickySet digit =
  if trickySetMatchesForDigit puzzle trickySet digit
    then
      -- XXX we could also check for new forced digits in
      -- the eliminate positions.
      -- We could remove the digit from the possibilities permanently,
      -- but that's not something a person would remember.  So just
      -- remove while we see if that creates a new placement.
      let eliminate = TrickySets.eliminate trickySet
          tmpPuzzle = Puzzle.notPossibleForList puzzle digit eliminate
      in trickySetCheckNeeded puzzle tmpPuzzle trickySet digit
    else
      Nothing

trickySetMatchesForDigit :: Puzzle -> TrickySet -> Int -> Bool
trickySetMatchesForDigit puzzle trickySet digit =
  let common = TrickySets.common trickySet
      rest = TrickySets.rest trickySet
  in (isDigitPossibleInSet puzzle digit common) &&
     (notIsDigitPossibleInSet puzzle digit rest)

trickySetCheckNeeded :: Puzzle -> Puzzle -> TrickySet -> Int -> Maybe Puzzle
trickySetCheckNeeded puzzle tmpPuzzle trickySet digit =
  let maybeUnknown =
        Solver.any
          (findUnknownWhereDigitIsNeeded tmpPuzzle digit)
          $ TrickySets.checkNeeded trickySet
  in case maybeUnknown of
       Nothing -> Nothing
       Just unknown ->
         let newPuzzle' = Puzzle.place puzzle unknown digit
             newPuzzle = debug
               ("T: " ++ show trickySet ++ "\n" ++
                "D: " ++ show digit ++ " " ++ show unknown ++ "\n" ++
                (Puzzle.toPuzzleString puzzle) ++
                (Puzzle.toPuzzleString newPuzzle'))
               newPuzzle'
         in Just $ newPuzzle

findUnknownWhereDigitIsNeeded :: Puzzle -> Int -> [Int] -> Maybe Unknown
findUnknownWhereDigitIsNeeded puzzle digit set =
  let unknowns = filter (isDigitPossibleForUnknown digit)
        $ filter (isUnknownInSet set)
        $ Puzzle.unknown puzzle
  in case unknowns of
       [unknown] -> Just unknown
       _ -> Nothing

isDigitPossibleInSet :: Puzzle -> Int -> [Int] -> Bool
isDigitPossibleInSet puzzle digit set =
  let possibleUnknowns =
        -- Filters can be in either order but this order is way faster.
        filter (isDigitPossibleForUnknown digit)
        $ filter (isUnknownInSet set)
        $ Puzzle.unknown puzzle
  in case possibleUnknowns of
       [] -> False
       _ -> True

-- XXX It is stinky-ass slow to use not $ isDigitPossibleInSet ...
-- Making this new function makes things 4 times faster solving
-- puzzle-1339.txt.
--
notIsDigitPossibleInSet :: Puzzle -> Int -> [Int] -> Bool
notIsDigitPossibleInSet puzzle digit set =
  let possibleUnknowns =
        -- Filters can be in either order but this order is way faster.
        filter (isDigitPossibleForUnknown digit)
        $ filter (isUnknownInSet set)
        $ Puzzle.unknown puzzle
  in case possibleUnknowns of
       [] -> True
       _ -> False

isDigitPossibleForUnknown :: Int -> Unknown -> Bool
isDigitPossibleForUnknown digit unknown =
  digit `elem` Unknown.possible unknown

isUnknownInSet :: [Int] -> Unknown -> Bool
isUnknownInSet list unknown =
  Unknown.cellNumber unknown `elem` list

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
firstMaybe :: [Maybe a] -> Maybe a
firstMaybe [] = Nothing
firstMaybe ((result @ (Just _)) : _) =
  result
firstMaybe (_ : tail) =
  firstMaybe tail

-- Call func with each element of the list and return the first result
-- that isn't Nothing.
--
any :: (a -> Maybe b) -> [a] -> Maybe b
any func list =
  firstMaybe $ map func list
