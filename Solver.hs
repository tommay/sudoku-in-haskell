module Solver (
  Solver.new,
  Solver.solutions,
  Solver.randomSolutions,
) where

import Digit (Digit)
import qualified Puzzle
import Placement (Placement (Placement))
import Puzzle (Puzzle)
import qualified Unknown
import Unknown (Unknown)
import qualified Placed
import qualified Solution
import Solution (Solution)
import qualified Stats
import Stats (Stats)
import Step (Step (Step))
import qualified ExclusionSet
import ExclusionSet (ExclusionSet (ExclusionSet))
import qualified TrickySets
import TrickySets (TrickySet)
import qualified Util

import qualified Data.List as List
import qualified System.Random as Random
import qualified System.Random.Shuffle as Shuffle
import Debug.Trace

tryHeuristics = True
tryTricky = False

doDebug = False

data Solver = Solver {
  puzzle :: Puzzle,
  rnd :: Maybe Random.StdGen,
  steps :: [Step],
  stats :: Stats
} deriving (Show)

data Next = Next Placement String (Stats -> Stats)

new :: Puzzle -> Maybe Random.StdGen -> Solver
new puzzle maybeRnd =
  Solver {
    puzzle = puzzle,
    rnd = maybeRnd,
    steps = [Step puzzle Nothing "Initial puzzle"],
    stats = Stats.new
  }

-- Try to solve the Puzzle, returning a list of Solutions.  This uses
-- tail-recursive style, passing down a list of solutions discovered
-- higher in the call stack.
--
solutions :: Puzzle -> [Solution]
solutions puzzle =
  let solver = Solver.new puzzle Nothing
  in solutionsTop solver []

randomSolutions :: Puzzle -> Random.StdGen -> [Solution]
randomSolutions puzzle rnd =
  let solver = Solver.new puzzle $ Just rnd
  in solutionsTop solver []

solutionsTop :: Solver -> [Solution] -> [Solution]
solutionsTop this results =
  case Puzzle.unknown $ Solver.puzzle this of
    [] ->
      -- No more unknowns, solved!
      Solution.new
        (Solver.puzzle this)
        (Solver.steps this)
        (Solver.stats this)
      : results
    _ -> solutionsHeuristic this results

solutionsHeuristic :: Solver -> [Solution] -> [Solution]
solutionsHeuristic this results =
  if tryHeuristics
    then -- Try the heuristic functions.
      let nextList = concat
            $ map (\ f -> f $ Solver.puzzle this)
            $ [placeEasyPeasy, placeOneMissing, placeOneNeeded, placeOneForced]
      in case nextList of
        (next : _) ->
          placeAndContinue this next results
        [] ->
          solutionsTricky this results
    else -- Skip the heuristics and continue with solutionsTricky.
      solutionsTricky this results

placeAndContinue :: Solver -> Next -> [Solution] -> [Solution]
placeAndContinue this next results =
  let Next placement description incStats = next
      Placement cellNumber digit = placement
      puzzle = Solver.puzzle this
      newPuzzle = Puzzle.place puzzle (Unknown.new cellNumber) digit
      step = Step newPuzzle (Just placement) description
      newSteps = (Solver.steps this) ++ [step]
      newStats = incStats $ Solver.stats this
      newSolver = this{ puzzle = newPuzzle, steps = newSteps, stats = newStats }
  in solutionsTop newSolver results

solutionsGuess :: Solver -> [Solution] -> [Solution]
solutionsGuess this results =
  -- We get here because we can't place a digit using human-style
  -- heuristics, so we've either failed or we have to guess and
  -- recurse.  We can distinguish by examining the cell with the
  -- fewest possibilities remaining, which is also the best cell to
  -- make a guess for.
  -- XXX May be faster to find minUnknown before even trying the heuristics
  -- so we can fail faster.  We may even chug along with the heuristics
  -- for a while before realizing we made a failing guess.

  let minUnknown = minByNumPossible $ Puzzle.unknown $ Solver.puzzle this
      possible = Unknown.possible minUnknown
  in case possible of
    [] ->
      -- Failed.  No more solutions.
      results
    [_] ->
      -- One possibility.  Recurse without incrementing guessCount.
      -- This will not happen if we're using the heuristics, but
      -- this case is included in case they're disabled.
      doGuesses this minUnknown possible results
    _ ->
      -- Multiple possibilities.  Guess each, maybe in a random order,
      -- and recurse.
      let shuffledPossible =
            case Solver.rnd this of
              Nothing -> possible
              Just rnd -> shuffle rnd possible
          newSolver = this{stats = Stats.guess $ Solver.stats this}
      in doGuesses newSolver minUnknown shuffledPossible results

-- For each digit in the list, use it as a guess for unknown
-- and try to solve the resulting Puzzle.
--
doGuesses :: Solver -> Unknown -> [Digit] -> [Solution] -> [Solution]
doGuesses this unknown digits results =
  foldr (\ digit accum ->
          let guess = Puzzle.place (Solver.puzzle this) unknown digit
          in solutionsTop this{puzzle = guess} accum)
    results
    digits

placeEasyPeasy :: Puzzle -> [Next]
placeEasyPeasy puzzle =
  concat $ map (easyPeasyStripe puzzle) $ makeEasyPeasyStripes

makeEasyPeasyStripes :: [(ExclusionSet, [ExclusionSet])]
makeEasyPeasyStripes =
  concat $ map makeEasyPeasyStripe
  $ Util.slices 3 (ExclusionSet.rows ++ ExclusionSet.columns)

makeEasyPeasyStripe :: [ExclusionSet] -> [(ExclusionSet, [ExclusionSet])]
makeEasyPeasyStripe slice =
  map (\ set -> (set, List.delete set slice)) slice

easyPeasyStripe :: Puzzle -> (ExclusionSet, [ExclusionSet]) -> [Next]
easyPeasyStripe puzzle (col0, [col1, col2]) =
  let digitsInCol1 = getDigitsInSet puzzle col1
      digitsInCol2 = getDigitsInSet puzzle col2
      easyPeasyDigits = (digitsInCol1 `List.intersect` digitsInCol2)
  in concat $ map (placeDigitInSet puzzle col0) easyPeasyDigits

getDigitsInSet :: Puzzle -> ExclusionSet -> [Digit]
getDigitsInSet puzzle set =
  map Placed.digit
  $ filter (\ p -> Placed.cellNumber p `elem` ExclusionSet.cells set)
  $ Puzzle.placed puzzle

placeDigitInSet :: Puzzle -> ExclusionSet -> Digit -> [Next]
placeDigitInSet puzzle set digit =
  let unknowns = unknownsInSet puzzle $ ExclusionSet.cells set
  in case filter (elem digit . Unknown.possible) unknowns of
      [unknown] -> [Next
                    (Placement (Unknown.cellNumber unknown) digit)
                    ("EasyPeasy " ++ ExclusionSet.name set)
                    id]
      _ -> []

-- Try to place a digit where a set has only one unplaced cell.
--
placeOneMissing :: Puzzle -> [Next]
placeOneMissing puzzle =
  concat $ map (placeOneMissingInSet puzzle) ExclusionSet.exclusionSets

placeOneMissingInSet :: Puzzle -> ExclusionSet -> [Next]
placeOneMissingInSet puzzle set =
  let ExclusionSet name cellNumbers = set
  in case unknownsInSet puzzle cellNumbers of
       [unknown] ->
         -- Exactly one cell in the set is unknown.  Place a digit in it.
         -- Note that since this is the only unknown position in the set
         -- there should be exactly one possible digit remaining.  But we
         -- may have made a wrong guess, which leaves no possibilities.
         case Unknown.possible unknown of
           [digit] -> [Next
                        (Placement (Unknown.cellNumber unknown) digit)
                        ("One missing in " ++ name)
                        incOneMissingInSet]
           [] -> []
       _ ->
         -- Zero or multiple cells in the set are unknown.
         []

incOneMissingInSet :: Stats -> Stats
incOneMissingInSet stats = stats  -- XXX

unknownsInSet :: Puzzle -> [Int] -> [Unknown]
unknownsInSet puzzle set =
  filter (isUnknownInSet set) $ Puzzle.unknown puzzle

-- Try to place a digit where there is a set doesn't yet have some
-- digit (it needs it) and there is only one cell in the set where it
-- can possibly go, and return Just Puzzle if a digit was placed.
-- This is pretty inefficient since it has to look through all the
-- digits and cells repeatedly but so what.
--
placeOneNeeded :: Puzzle -> [Next]
placeOneNeeded puzzle =
  concat $ map (placeOneNeededInSet puzzle) ExclusionSet.exclusionSets

placeOneNeededInSet :: Puzzle -> ExclusionSet -> [Next]
placeOneNeededInSet puzzle set =
  let ExclusionSet name cellNumbers = set
      unknowns = unknownsInSet puzzle cellNumbers
  in concat $ map (placeNeededDigitInSet puzzle unknowns name) [1..9]

placeNeededDigitInSet :: Puzzle -> [Unknown] -> String -> Digit -> [Next]
placeNeededDigitInSet puzzle unknowns name digit =
  case filter (isDigitPossibleForUnknown digit) unknowns of
    [unknown] -> [Next (Placement (Unknown.cellNumber unknown) digit)
                  ("Needed in " ++ name) id]
    _ -> []

placeOneForced :: Puzzle -> [Next]
placeOneForced puzzle =
  concat $ map (placeForcedUnknown puzzle) $ Puzzle.unknown puzzle

placeForcedUnknown :: Puzzle -> Unknown -> [Next]
placeForcedUnknown puzzle unknown =
  case Unknown.possible unknown of
    [digit] -> [Next (Placement (Unknown.cellNumber unknown) digit)
                "Forced" id]
    _ -> []

solutionsTricky :: Solver -> [Solution] -> [Solution]
solutionsTricky this results =
  if tryTricky
    then
      let puzzle = Solver.puzzle this
          maybePuzzle = concat $ map (tryTrickySet puzzle) TrickySets.trickySets
      in case maybePuzzle of
        (newPuzzle : _) ->
          solutionsTop this{puzzle = newPuzzle} results
        _ ->
          solutionsGuess this results
    else
      solutionsGuess this results

tryTrickySet :: Puzzle -> TrickySet -> [Puzzle]
tryTrickySet puzzle trickySet =
  concat $ map (tryTrickySetWithDigit puzzle trickySet) [1..9]

tryTrickySetWithDigit :: Puzzle -> TrickySet -> Digit -> [Puzzle]
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
      []

trickySetMatchesForDigit :: Puzzle -> TrickySet -> Digit -> Bool
trickySetMatchesForDigit puzzle trickySet digit =
  let common = TrickySets.common trickySet
      rest = TrickySets.rest trickySet
  in (isDigitPossibleInSet puzzle digit common) &&
     (notIsDigitPossibleInSet puzzle digit rest)

trickySetCheckNeeded :: Puzzle -> Puzzle -> TrickySet -> Digit -> [Puzzle]
trickySetCheckNeeded puzzle tmpPuzzle trickySet digit =
  let maybeUnknown =
        concat $ map
          (findUnknownsWhereDigitIsNeeded tmpPuzzle digit)
          $ TrickySets.checkNeeded trickySet
  in case maybeUnknown of
       [unknown] ->
         let newPuzzle' = Puzzle.place puzzle unknown digit
             newPuzzle = debug
               ("T: " ++ show trickySet ++ "\n" ++
                "D: " ++ show digit ++ " " ++ show unknown ++ "\n" ++
                (Puzzle.toPuzzleString puzzle) ++
                (Puzzle.toPuzzleString newPuzzle'))
               newPuzzle'
         in [newPuzzle]
       _ -> []

findUnknownsWhereDigitIsNeeded :: Puzzle -> Digit -> [Int] -> [Unknown]
findUnknownsWhereDigitIsNeeded puzzle digit set =
  filter (isDigitPossibleForUnknown digit)
    $ filter (isUnknownInSet set)
    $ Puzzle.unknown puzzle

isDigitPossibleInSet :: Puzzle -> Digit -> [Int] -> Bool
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
notIsDigitPossibleInSet :: Puzzle -> Digit -> [Int] -> Bool
notIsDigitPossibleInSet puzzle digit set =
  let possibleUnknowns =
        -- Filters can be in either order but this order is way faster.
        filter (isDigitPossibleForUnknown digit)
        $ filter (isUnknownInSet set)
        $ Puzzle.unknown puzzle
  in case possibleUnknowns of
       [] -> True
       _ -> False

isDigitPossibleForUnknown :: Digit -> Unknown -> Bool
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

-- Debug function that outputs only if doDebug is True.
--
debug :: String -> b -> b
debug a b =
  if doDebug
    then trace a b
    else b
