module Puzzle
( Puzzle
, Puzzle.getSolutions
, Puzzle.toPuzzleString
) where  

import qualified Cells
import Cells (Cells)
import qualified Cell
import qualified Possible
import qualified Exclusions
import Exclusions (Exclusions)

import qualified Data.Char as Char
import qualified System.Random as Random
import qualified System.Random.Shuffle as Shuffle

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

-- Try to solve this Puzzle, returning a list of solved Puzzles.
--
solve :: Puzzle -> Random.StdGen -> [Puzzle] -> [Puzzle]
solve this gen solutions =
  -- We get here either because we're done, we've failed, or
  -- we have to guess and recurse.  We can distinguish by
  -- examining the unplaced cell with the fewest possibilities
  -- remaining.
  let minCell = Cells.minByPossibleSize $ cells this
  in case Cell.getPossible minCell of
       Nothing ->
         -- Solved.  Yield this as a solution.
         this:solutions
       Just possible ->
         if Possible.size possible == 0
           then
             -- Failed.  No solution to yield.
             solutions
           else
             -- Found an unplaced cell with two or more possibilities.
             -- Guess each possibility and recurse.
             let shuffledList = shuffleList gen $ Possible.toList possible
             in doGuesses this gen solutions (Cell.getNumber minCell) shuffledList

shuffleList :: Random.StdGen -> [a] -> [a]
shuffleList gen list = do
  Shuffle.shuffle' list (length list) gen

-- For each Digit in the list, use it as a guess for cellNumber
-- and try to solve the resulting Puzzle.
--
doGuesses :: Puzzle -> Random.StdGen -> [Puzzle] -> Int -> [Int] -> [Puzzle]
doGuesses this gen solutions cellNumber digits =
  foldr (\ digit accum ->
          let guess = Puzzle.place this cellNumber digit
          in solve guess gen accum)
    solutions
    digits

getSolutions :: String -> Random.StdGen-> [Puzzle]
getSolutions setup gen =
  Puzzle.solve (Puzzle.new setup) gen []

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
  let (slice, rest) = splitAt n list
  in slices' n rest (accum ++ [slice])
