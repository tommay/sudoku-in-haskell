module Solver (
  Solver.new,
  Solver.solutions,
  Solver.randomSolutions,
) where

import Digit (Digit)
import qualified EasyPeasy
import Next (Next (Next))
import qualified Next
import Placement (Placement (Placement))
import qualified Puzzle
import Puzzle (Puzzle)
import qualified Unknown
import Unknown (Unknown)
import qualified Placed
import qualified Solution
import Solution (Solution)
import qualified SolverUtil
import qualified Stats
import Stats (Stats)
import Step (Step (Step))
import qualified ExclusionSet
import ExclusionSet (ExclusionSet (ExclusionSet))
import qualified TrickySets
import TrickySets (TrickySet)

import qualified Data.List as List
import qualified System.Random as Random
import qualified System.Random.Shuffle as Shuffle
import Debug.Trace

useHeuristics = True
useEasyPeasy = False
useMissingOne = False
useMissingTwo = False
useNeeded = False
useForced = False  -- This is actually hard for people.
useTricky = True

doDebug = False

data Solver = Solver {
  puzzle :: Puzzle,
  rnd :: Maybe Random.StdGen,
  steps :: [Step],
  stats :: Stats
} deriving (Show)

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

-- This computes all the solutions but they're returned in a random
-- order.
--
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
  if useHeuristics
    then -- Try the heuristic functions.
      let (rnd1, rnd2) = maybeSplit $ Solver.rnd this
          -- This uses rnd1 to shuffle each function's list, but that's ok
          -- because only the first Next is used.
          nextList = concat
            $ map (maybeShuffle rnd1)
            $ map (\ f -> f $ Solver.puzzle this)
            $ heuristics
      in case nextList of
        (next : _) ->
          placeAndContinue this{rnd = rnd2} next results
        [] ->
          solutionsGuess this results
    else -- Skip the heuristics and continue with solutionsGuess.
      solutionsGuess this results

heuristics :: [Puzzle -> [Next]]
heuristics =
 let heuristic bool func = if bool then [func] else []
 in concat [
   -- Heuristic methods to find placements, ordered from easiest to hardest
   -- for people to do.  Easy placements are used preferentially.
   heuristic useEasyPeasy EasyPeasy.find,
   heuristic useMissingOne findMissingOne,
   heuristic useMissingTwo findMissingTwo,
   heuristic useNeeded findNeeded,
   heuristic useForced findForced,
   heuristic useTricky findTricky
 ]

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
      -- and recurse.  We could use Random.split when shuffling or
      -- recursing, but it's not really important for this application.
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

-- Try to place a digit where a set has only one unplaced cell.
--
findMissingOne :: Puzzle -> [Next]
findMissingOne puzzle =
  concat $ map (findMissingOneInSet puzzle) ExclusionSet.exclusionSets

findMissingOneInSet :: Puzzle -> ExclusionSet -> [Next]
findMissingOneInSet puzzle set =
  let ExclusionSet name cellNumbers = set
  in case SolverUtil.unknownsInSet puzzle cellNumbers of
       [unknown] ->
         -- Exactly one cell in the set is unknown.  Place a digit in it.
         -- Note that since this is the only unknown position in the set
         -- there should be exactly one possible digit remaining.  But we
         -- may have made a wrong guess, which leaves no possibilities.
         case Unknown.possible unknown of
           [digit] -> [Next.new ("Missing one in " ++ name) incMissingOneInSet
                       digit unknown]
           [] -> []
       _ ->
         -- Zero or multiple cells in the set are unknown.
         []

incMissingOneInSet :: Stats -> Stats
incMissingOneInSet stats = stats  -- XXX

-- Try to place a digit where a set has two unplaced cells.  We only
-- place one of the digits but the second will follow quickly.
--
findMissingTwo :: Puzzle -> [Next]
findMissingTwo puzzle =
  concat $ map (findMissingTwoInSet puzzle) ExclusionSet.exclusionSets

findMissingTwoInSet :: Puzzle -> ExclusionSet -> [Next]
findMissingTwoInSet puzzle set =
  let ExclusionSet name cellNumbers = set
  in case SolverUtil.unknownsInSet puzzle cellNumbers of
       unknowns@[_, _] ->
         concat $
           map (findForcedForUnknown puzzle ("Missing two in " ++ name))
           unknowns
       _ ->
         []

-- Try to place a digit where there is a set that doesn't yet have
-- some digit (i.e., it needs it) and there is only one cell in the
-- set where it can possibly go.
--
findNeeded :: Puzzle -> [Next]
findNeeded puzzle =
  concat $ map (findNeededInSet puzzle) ExclusionSet.exclusionSets

findNeededInSet :: Puzzle -> ExclusionSet -> [Next]
findNeededInSet puzzle set =
  let ExclusionSet name cellNumbers = set
      unknowns = SolverUtil.unknownsInSet puzzle cellNumbers
  in concat $ map (findNeededDigitInSet puzzle unknowns name) [1..9]

findNeededDigitInSet :: Puzzle -> [Unknown] -> String -> Digit -> [Next]
findNeededDigitInSet puzzle unknowns name digit =
  case filter (isDigitPossibleForUnknown digit) unknowns of
    [unknown] -> [Next.new
                  (unwords ["Need a", show digit, "in", name])
                  id digit unknown]
    _ -> []

findForced :: Puzzle -> [Next]
findForced puzzle =
  concat $ map (findForcedForUnknown puzzle "Forced") $ Puzzle.unknown puzzle

findForcedForUnknown :: Puzzle -> String -> Unknown -> [Next]
findForcedForUnknown puzzle description unknown =
  case Unknown.possible unknown of
    [digit] -> [Next.new description id digit unknown]
    _ -> []

findTricky :: Puzzle -> [Next]
findTricky puzzle =
  concat $ map (findTrickySet puzzle) TrickySets.trickySets

findTrickySet :: Puzzle -> TrickySet -> [Next]
findTrickySet puzzle trickySet =
  concat $ map (findTrickySetWithDigit puzzle trickySet) [1..9]

findTrickySetWithDigit :: Puzzle -> TrickySet -> Digit -> [Next]
findTrickySetWithDigit puzzle trickySet digit =
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

trickySetCheckNeeded :: Puzzle -> Puzzle -> TrickySet -> Digit -> [Next]
trickySetCheckNeeded puzzle tmpPuzzle trickySet digit =
  let unknownForEachNeededSet =
        concat $ map
          (findUnknownWhereDigitIsNeeded tmpPuzzle digit)
          $ TrickySets.checkNeeded trickySet
  in map (Next.new
           (TrickySets.name trickySet)
           id digit) unknownForEachNeededSet

findUnknownWhereDigitIsNeeded :: Puzzle -> Digit -> [Int] -> [Unknown]
findUnknownWhereDigitIsNeeded puzzle digit set =
  let unknowns = filter (isDigitPossibleForUnknown digit)
        $ filter (SolverUtil.isUnknownInSet set)
        $ Puzzle.unknown puzzle
  in case unknowns of
    [_] -> unknowns
    _ -> []

isDigitPossibleInSet :: Puzzle -> Digit -> [Int] -> Bool
isDigitPossibleInSet puzzle digit set =
  let possibleUnknowns =
        -- Filters can be in either order but this order is way faster.
        filter (isDigitPossibleForUnknown digit)
        $ filter (SolverUtil.isUnknownInSet set)
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
        $ filter (SolverUtil.isUnknownInSet set)
        $ Puzzle.unknown puzzle
  in case possibleUnknowns of
       [] -> True
       _ -> False

isDigitPossibleForUnknown :: Digit -> Unknown -> Bool
isDigitPossibleForUnknown digit unknown =
  digit `elem` Unknown.possible unknown

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
-- shuffle' infinite loops on [], grr.
shuffle _ [] = []
shuffle rnd list =
  Shuffle.shuffle' list (length list) rnd

myShuffle :: Random.StdGen -> [a] -> [a]
myShuffle rnd [] = []
myShuffle rnd list =
  let len = length list
      (n, newRnd) = Random.randomR (0, len - 1) rnd
      (first, (e : rest)) = List.splitAt n list
  in e : (myShuffle newRnd $ first ++ rest)

maybeSplit :: Maybe Random.StdGen -> (Maybe Random.StdGen, Maybe Random.StdGen)
maybeSplit Nothing =
  (Nothing, Nothing)
maybeSplit (Just rnd) =
  let (rnd1, rnd2) = Random.split rnd
  in (Just rnd1, Just rnd2)

maybeShuffle :: Maybe Random.StdGen -> [a] -> [a]
maybeShuffle Nothing list = list
maybeShuffle (Just rnd) list =
  shuffle rnd list

-- Debug function that outputs only if doDebug is True.
--
debug :: String -> b -> b
debug a b =
  if doDebug
    then trace a b
    else b
