module Puzzle
( Puzzle
, Puzzle.getSolutions
, Puzzle.toPuzzleString
) where  

import Cells
import Cell
import Possible
import Exclusions

import Data.Char as Char

data Puzzle = Puzzle {
    cells :: Cells
  , exclusions :: Exclusions
  } deriving (Show)

-- Returns a new Puzzle with empty Cells.
--
empty :: Puzzle
empty =
  Puzzle {cells = Cells.new,
          exclusions = Exclusions.new}

-- Returns a new Puzzle with each Cell initialized according to
-- Setup, which is a string of 81 digits or dashes.
--
new :: String -> Puzzle
new setup =
  let digits = toDigits setup
      zipped = zip digits [0..80]
  in foldr (\ (digit, cellNumber) this ->
            case digit of
              Nothing -> this
              Just digit -> Puzzle.place this cellNumber digit)
       empty
       zipped

-- Given a Setup string, returns a list of Maybe Int for
-- each cell.
--
toDigits :: String -> [Maybe Int]
toDigits setup =
  [case char of
    '-' -> Nothing
    _ -> Just $ Char.digitToInt char
   | char <- setup]

-- Returns a new Puzzle with Digit placed in Cell AtNumber.  The
-- possible sets of all Cells are updated to account for the new
-- placement.
--
place :: Puzzle -> Int -> Int -> Puzzle
place this cellNumber digit =
  let -- Place the Digit.
      cells1 = Cells.update
                 (cells this)
                 cellNumber
                 (\ cell -> Cell.place cell digit)
      -- Exclude Digit from excluded Cells.
      exclusionList = Exclusions.getListForCell
                        (exclusions this)
                        cellNumber
      cells2 = Cells.doExclusions cells1 digit exclusionList
  in this {cells = cells2}

-- Try to solve this Puzzle then report back solved or failed to the
-- Collector, and possibly spawn further processes that also report
-- back to the Collector.
--
solve :: Puzzle -> Collector -> Collector
solve this collector =
  -- We get here either because we're done, we've failed, or
  -- we have to guess and recurse.  We can distinguish by
  -- examining the unplaced cell with the fewest possibilities
  -- remaining.
  let minCell = Cells.minByPossibleSize $ cells this
  in case Cell.getPossible minCell of
       Nothing ->
         -- Solved.  Yield this as a solution.
         yieldSolution collector this
       Just possible ->
         if Possible.size possible == 0
           then
             -- Failed.  No solution to yield.
             collector
           else
             -- Found an unplaced cell with two or more possibilities.
             -- Guess each possibility and recurse.
             doGuesses this collector (Cell.getNumber minCell)
               (Possible.toList possible)

-- For each Digit in the list, use it as a guess for cellNumber
-- and try to solve the resulting Puzzle.
--
doGuesses :: Puzzle -> Collector -> Int -> [Int] -> Collector
doGuesses this collector cellNumber digits =
  foldr (\ digit accum ->
          solve (Puzzle.place this cellNumber digit) accum)
    collector digits

getSolutions :: String -> [Puzzle]
getSolutions setup =
  let puzzle = Puzzle.new setup
  in getCollectedSolutions $ Puzzle.solve puzzle Puzzle.newCollector

-- Returns a raw string of 81 digits and dashes, like the argument to
-- new.
--
toString :: Puzzle -> String
toString this =
  Cells.toString $ cells this

-- Returns a string that prints out as a grid of digits.
--
toPuzzleString :: Puzzle -> String
toPuzzleString this =
  unlines $
    map (\puzzles ->
          unlines $
            map (\row -> unwords $ slices 3 row) $
              slices 9 puzzles) $
      slices 27 $ Puzzle.toString this

-- You'd think there would be a function to do this but I can't
-- find one easily.  It will be good practice to roll my own.
-- This can be done all kinds of ways, but here I don't use
-- anything fancy.   Well ok splitAt is fancy.  And I'm building it
-- non-reversed with ++ instead of :.
--
slices :: Int -> [a] -> [[a]]
slices n list =
  slices' n list []

slices' :: Int -> [a] -> [[a]] -> [[a]]
slices' n [] accum = accum
slices' n list accum =
  let (slice, rest) = Prelude.splitAt n list
  in slices' n rest (accum ++ [slice])

----- This should really go in its own module, but mutually recursive
----- modules are a major PITA in ghc.

data Collector = Collector [Puzzle]

newCollector :: Collector
newCollector =
  Collector []

yieldSolution :: Collector -> Puzzle -> Collector
yieldSolution (Collector list) puzzle =
  Collector $ puzzle : list

getCollectedSolutions :: Collector -> [Puzzle]
getCollectedSolutions (Collector list) = list
