module Solver (
  Solver.solutions,
  Solver.randomSolutions,
  Solver.allRandomSolutions,
  Solver.isSolvableWith,
) where

import           Digit (Digit)
import qualified EasyPeasy
import qualified ExclusionSet
import           ExclusionSet (ExclusionSet (ExclusionSet))
import qualified Next
import           Next (Next (Next))
import           Placement (Placement (Placement))
import qualified Puzzle
import           Puzzle (Puzzle)
import qualified Solution
import           Solution (Solution)
import qualified SolverOptions
import           SolverOptions (SolverOptions)
import           SolverOptions (Heuristic (..))
import qualified SolverUtil
import           Step (Step (Step))
import qualified TrickySet
import           TrickySet (TrickySet)
import qualified Unknown
import           Unknown (Unknown)
import qualified Util

import qualified Data.Bits as Bits
import qualified Data.List as List
import qualified System.Random as Random
import Debug.Trace

doDebug = False

data Solver = Solver {
  options :: SolverOptions,
  puzzle :: Puzzle,
  rnd :: Maybe Random.StdGen,
  unknowns :: [Unknown],
  steps :: [Step]
} deriving (Show)

new :: SolverOptions -> Maybe Random.StdGen -> Puzzle -> Solver
new options' maybeRnd puzzle =
  let (rnd1, rnd2) = maybeSplit maybeRnd
      emptySolver = Solver.Solver {
        options = options',
        puzzle = Puzzle.empty,
        rnd = rnd1,
        unknowns = maybeShuffle rnd2 [Unknown.new n | n <- [0..80]],
        steps = [Step puzzle Nothing "Initial puzzle"]
      }
  in foldr (\ (cellNumber, digit) accum ->
       place accum cellNumber digit)
     emptySolver
     $ Puzzle.each puzzle

place :: Solver -> Int -> Digit -> Solver
place this cellNumber digit =
  let newPuzzle = Puzzle.place (Solver.puzzle this) cellNumber digit
      newUnknowns = map (Unknown.place cellNumber digit)
        $ filter ((/= cellNumber) . Unknown.cellNumber)
        $ Solver.unknowns this
  in this{ puzzle = newPuzzle, unknowns = newUnknowns }

-- Try to solve the Puzzle, returning a list of Solutions.  This uses
-- tail-recursive style, passing down a list of solutions discovered
-- higher in the call stack.
--
solutions :: SolverOptions -> Puzzle -> [Solution]
solutions options puzzle =
  let solver = Solver.new options Nothing puzzle
  in solutionsTop solver []

randomSolutions :: SolverOptions -> Random.StdGen -> Puzzle -> [Solution]
randomSolutions options rnd puzzle =
  let solver = Solver.new options (Just rnd) puzzle
  in solutionsTop solver []

-- Compute all the solutions and return them in a random order.
--
allRandomSolutions :: Random.StdGen -> Puzzle -> [Solution]
allRandomSolutions = randomSolutions SolverOptions.all

solutionsTop :: Solver -> [Solution] -> [Solution]
solutionsTop this results =
  case Solver.unknowns this of
    [] ->
      -- No more unknowns, solved!
      Solution.new
        (Solver.puzzle this)
        (Solver.steps this)
      : results
    _ -> solutionsHeuristic this results

solutionsHeuristic :: Solver -> [Solution] -> [Solution]
solutionsHeuristic this results =
  if SolverOptions.useHeuristics $ Solver.options this
    then -- Try the heuristic functions.
      let (rnd1, rnd2) = maybeSplit $ Solver.rnd this
          -- This uses rnd1 to shuffle each function's list, but that's ok
          -- because only the first Next is used.
          nextList = concat
            $ map (maybeShuffle rnd1)
            $ map (\ f -> f this)
            $ heuristics this
      in case nextList of
        next : _ ->
          placeAndContinue this{rnd = rnd2} next results
        [] ->
          solutionsStuck this results
    else -- Skip the heuristics and continue with solutionsStuck.
      solutionsStuck this results

heuristics :: Solver -> [Solver -> [Next]]
heuristics this =
  map (\ h -> case h of
        EasyPeasy -> findEasyPeasy
        MissingOne -> findMissingOne
        MissingTwo -> findMissingTwo
        Tricky -> findTricky
        Needed -> findNeeded
        Forced -> findForced)
  $ SolverOptions.heuristics $ Solver.options this

placeAndContinue :: Solver -> Next -> [Solution] -> [Solution]
placeAndContinue this next results =
  let Next placement description = next
      Placement cellNumber digit = placement
      newSolver = place this cellNumber digit
      step = Step (Solver.puzzle newSolver) (Just placement) description
      newSteps = (Solver.steps this) ++ [step]
      newSolver2 = newSolver{ steps = newSteps }
  in solutionsTop newSolver2 results

solutionsStuck :: Solver -> [Solution] -> [Solution]
solutionsStuck this results =
  -- We get here because we can't place a digit using human-style
  -- heuristics, so we've either failed or we have to guess and
  -- recurse.  We can distinguish by examining the cell with the
  -- fewest possibilities remaining, which is also the best cell to
  -- make a guess for.
  let minUnknown = minBy Unknown.numPossible $ Solver.unknowns this
      cellNumber = Unknown.cellNumber minUnknown
      possible = Unknown.getPossible minUnknown
      options = Solver.options this
  in case possible of
    [] ->
      -- Failed.  No more solutions.
      results
    [digit] ->
      -- One possibility.  The choice is forced, no guessing.  But we
      -- only use the force if a) we're guessing, b) we're not using
      -- heuristics, because if we are then forcing is done by
      -- findForced.
      if (SolverOptions.useGuessing options) &&
         (not $ SolverOptions.useHeuristics options)
        then let next = Next.new "Forced guess" digit cellNumber
             in placeAndContinue this next results
        else -- There is a forced guess but we're not configured to
             -- use it.  See if we can permanently apply a TrickySet
             -- to create an opportunity.
             case applyOneTrickySetIfAllowed this of
               Just newSolver -> solutionsTop newSolver results
               Nothing -> results
    _ ->
      -- Multiple possibilities.  Before we guess, see if it's possible
      -- to permanently apply a TrickySet to create possibilities for
      -- heuristics.
      case applyOneTrickySetIfAllowed this of
        Just newSolver -> solutionsTop newSolver results
        Nothing -> if SolverOptions.useGuessing options
          -- Guess each possibility, maybe in a random order, and
          -- recurse.  We could use Random.split when shuffling or
          -- recursing, but it's not really important for this
          -- application.
          then let shuffledPossible = maybeShuffle (Solver.rnd this) possible
               in doGuesses this cellNumber shuffledPossible results
          else results

-- For each digit in the list, use it as a guess for unknown
-- and try to solve the resulting Puzzle.
--
doGuesses :: Solver -> Int -> [Digit] -> [Solution] -> [Solution]
doGuesses this cellNumber digits results =
  foldr (\ digit accum ->
          let next = Next.new "Guess" digit cellNumber
          in placeAndContinue this next accum)
    results
    digits

findEasyPeasy :: Solver -> [Next]
findEasyPeasy this =
  EasyPeasy.find (Solver.puzzle this) (Solver.unknowns this)

-- Try to place a digit where a set has only one Unknown.
--
findMissingOne :: Solver -> [Next]
findMissingOne this =
  concat $ map (findMissingOneInSet this) ExclusionSet.exclusionSets

findMissingOneInSet :: Solver -> ExclusionSet -> [Next]
findMissingOneInSet this set =
  let ExclusionSet name cellNumbers = set
  in case SolverUtil.unknownsInSet (Solver.unknowns this) cellNumbers of
       [unknown] ->
         -- Exactly one cell in the set is unknown.  Place a digit in it.
         -- Note that since this is the only unknown position in the set
         -- there should be exactly one possible digit remaining.  But we
         -- may have made a wrong guess, which leaves no possibilities.
         findForcedForUnknown ("Missing one in " ++ name) unknown
       _ ->
         -- Zero or multiple cells in the set are unknown.
         []

-- Try to place a digit where a set has two unplaced cells.  We only
-- place one of the digits but the second will follow quickly.
--
findMissingTwo :: Solver -> [Next]
findMissingTwo this =
  concat $ map (findMissingTwoInSet this) ExclusionSet.exclusionSets

findMissingTwoInSet :: Solver -> ExclusionSet -> [Next]
findMissingTwoInSet this set =
  let ExclusionSet name cellNumbers = set
  in case SolverUtil.unknownsInSet (Solver.unknowns this) cellNumbers of
       unknowns@[_, _] ->
         concat $
           map (findForcedForUnknown $ "Missing two in " ++ name) unknowns
       _ -> []

-- Try to place a digit where there is a set that doesn't yet have
-- some digit (i.e., it needs it) and there is only one cell in the
-- set where it can possibly go.
--
findNeeded :: Solver -> [Next]
findNeeded this =
  concat $ map (findNeededInSet this) ExclusionSet.exclusionSets

-- Rather than check all digits, it's actually a bit faster to take the
-- trouble to get possibleDigitList.
--
findNeededInSet :: Solver -> ExclusionSet -> [Next]
findNeededInSet this set =
  let ExclusionSet name cells = set
      us = SolverUtil.unknownsInSet (Solver.unknowns this) cells
      possibleInSet = List.foldl'
        (\ accum u -> accum Bits..|. Unknown.possible u) 0 us
      possibleDigitList = Unknown.getPossibleList possibleInSet
      makeDescription digit = unwords ["Need a", show digit, "in", name]
  in concat $
       map (findNeededDigitInUnknowns us makeDescription) possibleDigitList

findNeededDigitInUnknowns :: [Unknown] -> (Digit -> String) -> Digit -> [Next]
findNeededDigitInUnknowns unknowns makeDescription digit =
  case filter (Unknown.isDigitPossible digit) unknowns of
    [unknown] -> [Next.new
                  (makeDescription digit)
                  digit (Unknown.cellNumber unknown)]
    _ -> []

findForced :: Solver -> [Next]
findForced this =
  concat $ map (findForcedForUnknown "Forced") $ Solver.unknowns this

findForcedForUnknown :: String -> Unknown -> [Next]
findForcedForUnknown description unknown =
  if Unknown.numPossible unknown == 1
    then let digit = head $ Unknown.getPossible unknown
         in [Next.new description digit (Unknown.cellNumber unknown)]
    else []

findTricky :: Solver -> [Next]
findTricky this =
  let unknowns = Solver.unknowns this
      applicableTrickySets = findApplicableTrickySets unknowns
  in concat $ map
       (\ (digit, trickySet) ->
         -- XXX we could also check for new forced digits in the
         -- eliminate positions.  We could remove the digit from the
         -- possibilities permanently, but that's not something a
         -- person would remember unless they're using paper.  So just
         -- remove while we see if that creates a new placement.
         let tmpUnknowns = eliminateWithTrickySet unknowns digit trickySet
         in trickySetCheckNeeded tmpUnknowns trickySet digit)
       applicableTrickySets

trickySetCheckNeeded :: [Unknown] -> TrickySet -> Digit -> [Next]
trickySetCheckNeeded unknowns trickySet digit =
  let unknownForEachNeededSet =
        concat $ map
          (findUnknownWhereDigitIsNeeded unknowns digit)
          $ TrickySet.checkNeeded trickySet
  in map (Next.new
           (TrickySet.name trickySet)
           digit . Unknown.cellNumber) unknownForEachNeededSet

trickySetMatchesForDigit :: [Unknown] -> TrickySet -> Digit -> Bool
trickySetMatchesForDigit unknowns trickySet digit =
  let common = TrickySet.common trickySet
      rest = TrickySet.rest trickySet
  in (isDigitPossibleInSet unknowns digit common) &&
     (notIsDigitPossibleInSet unknowns digit rest)

findUnknownWhereDigitIsNeeded :: [Unknown] -> Digit -> [Int] -> [Unknown]
findUnknownWhereDigitIsNeeded unknowns digit set =
  let unknowns' = filter (Unknown.isDigitPossible digit)
        $ filter (SolverUtil.isUnknownInSet set) unknowns
  in case unknowns' of
    [_] -> unknowns'
    _ -> []

isDigitPossibleInSet :: [Unknown] -> Digit -> [Int] -> Bool
isDigitPossibleInSet unknowns digit set =
  let possibleUnknowns =
        -- Filters can be in either order but this order is way faster.
        -- XXX is that still true now that possible it a bitmap?
        filter (Unknown.isDigitPossible digit)
        $ filter (SolverUtil.isUnknownInSet set) unknowns
  in case possibleUnknowns of
       [] -> False
       _ -> True

-- XXX It is stinky-ass slow to use not $ isDigitPossibleInSet ...
-- Making this new function makes things 4 times faster solving
-- puzzle-1339.txt.
--
notIsDigitPossibleInSet :: [Unknown] -> Digit -> [Int] -> Bool
notIsDigitPossibleInSet unknowns digit set =
  let possibleUnknowns =
        -- Filters can be in either order but this order is way faster.
        -- XXX is that still true now that possible it a bitmap?
        filter (Unknown.isDigitPossible digit)
        $ filter (SolverUtil.isUnknownInSet set) unknowns
  in case possibleUnknowns of
       [] -> True
       _ -> False

applyOneTrickySetIfAllowed :: Solver -> Maybe Solver
applyOneTrickySetIfAllowed this =
  if SolverOptions.usePermanentTrickySets $ Solver.options this
    then applyOneTrickySet this
    else Nothing

-- Try all applicable TrickyuSets in a random order until one makes a
-- difference and return a new Solver with some possibilities
-- eliminated, or Nothing.  This is vastly inefficient, but it's only
-- used when grading puzzles for difficulty.
--
applyOneTrickySet :: Solver -> Maybe Solver
applyOneTrickySet this =
  let (rnd1, rnd2) = maybeSplit $ Solver.rnd this
      applicableTrickySets = maybeShuffle rnd1
        $ findApplicableTrickySets $ Solver.unknowns this
      tryTrickySet (digit, trickySet) =
        applyTrickySet this digit trickySet
  in case concat $ map tryTrickySet applicableTrickySets of
       solver : _ -> Just solver{ rnd = rnd2 }
       _ -> Nothing

applyTrickySet :: Solver -> Digit -> TrickySet -> [Solver]
applyTrickySet this digit trickySet =
  let oldUnknowns = Solver.unknowns this
      newUnknowns = eliminateWithTrickySet oldUnknowns digit trickySet
  in if newUnknowns /= oldUnknowns
       then
         let newSolver = Solver.addStep this
               (Step (Solver.puzzle this) Nothing
                 ("Apply " ++ TrickySet.name trickySet))
         in [newSolver{ unknowns = newUnknowns }]
       else []

findApplicableTrickySets :: [Unknown] -> [(Digit, TrickySet)]
findApplicableTrickySets unknowns =
  let allTrickySets = TrickySet.trickySets ++ TrickySet.inverseTrickySets
  in [(digit, trickySet) | digit <- [1..9], trickySet <- allTrickySets,
      trickySetMatchesForDigit unknowns trickySet digit]

eliminateWithTrickySet :: [Unknown] -> Digit -> TrickySet -> [Unknown]
eliminateWithTrickySet unknowns digit trickySet =
  let cellNumbers = TrickySet.eliminate trickySet
  in map (\ u ->
          if Unknown.cellNumber u `elem` cellNumbers
             then Unknown.removeDigitFromPossible digit u
             else u)
       unknowns

addStep :: Solver -> Step -> Solver
addStep this step =
  this{ steps = Solver.steps this ++ [step] }

isSolvableWith :: SolverOptions -> Puzzle -> Bool
isSolvableWith options puzzle =
  not $ null $ Solver.solutions options puzzle

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

maybeSplit :: Maybe Random.StdGen -> (Maybe Random.StdGen, Maybe Random.StdGen)
maybeSplit Nothing =
  (Nothing, Nothing)
maybeSplit (Just rnd) =
  let (rnd1, rnd2) = Random.split rnd
  in (Just rnd1, Just rnd2)

maybeShuffle :: Maybe Random.StdGen -> [a] -> [a]
maybeShuffle Nothing list = list
maybeShuffle (Just rnd) list =
  Util.shuffle rnd list

-- Debug function that outputs only if doDebug is True.
--
debug :: String -> b -> b
debug a b =
  if doDebug
    then trace a b
    else b

spud text val = trace (text ++ ": " ++ (show val)) val
