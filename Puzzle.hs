module Puzzle
(
  Puzzle,
  Puzzle.empty,
  Puzzle.remove,
  Puzzle.solutions,
  Puzzle.randomSolutions,
  Puzzle.solutionsFor,
  Puzzle.toPuzzleString,
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
  cells :: Cells,
  exclusions :: Exclusions
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
-- possible sets of all Cells are adjustd to account for the new
-- placement.
--
place :: Puzzle -> Int -> Int -> Puzzle
place this cellNumber digit =
  let -- Place the Digit.
      cells1 = Cells.adjust
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
solutions :: Puzzle -> [(Int, Puzzle)]
solutions this =
  solutions' this Nothing 0 []

randomSolutions :: Puzzle -> Random.StdGen -> [(Int, Puzzle)]
randomSolutions this rnd =
  solutions' this (Just rnd) 0 []

-- Try to solve this Puzzle, returning a list of solved Puzzles.
--
solutions' :: Puzzle -> Maybe Random.StdGen -> Int -> [(Int, Puzzle)] -> [(Int, Puzzle)]
solutions' this maybeGen guesses results =
  -- We get here either because we're done, we've failed, or
  -- we have to guess and recurse.  We can distinguish by
  -- examining the unplaced cell with the fewest possibilities
  -- remaining.
  let minCell = Cells.minByPossibleSize $ cells this
  in case Cell.getPossible minCell of
       Nothing ->
         -- Solved.  Yield this as a solution.
         (guesses, this):results
       Just possible ->
         case Possible.toList possible of
           [] ->
             -- Failed.  No more solutions.
             results
           possibleDigits@[_] ->
             -- One possibility.  Recurse without incrementing guesses.
             let cellNumber = Cell.getNumber minCell
             in doGuesses this maybeGen
                  guesses results cellNumber possibleDigits
           possibleDigits ->
             -- Multiple possibilities.  Guess each, maybe in a random order,
             -- and recurse.
             let cellNumber = Cell.getNumber minCell
                 shuffledList =
                   case maybeGen of
                     Nothing -> possibleDigits
                     Just rnd -> shuffle rnd possibleDigits
             in doGuesses this maybeGen
                  (guesses + 1) results cellNumber shuffledList

-- For each Digit in the list, use it as a guess for cellNumber
-- and try to solve the resulting Puzzle.
--
doGuesses :: Puzzle -> Maybe Random.StdGen -> Int -> [(Int, Puzzle)] -> Int -> [Int] -> [(Int, Puzzle)]
doGuesses this maybeGen guesses results cellNumber digits =
  foldr (\ digit accum ->
          let guess = Puzzle.place this cellNumber digit
          in solutions' guess maybeGen guesses accum)
    results
    digits

solutionsFor :: String -> [(Int, Puzzle)]
solutionsFor setup =
  solutions $ Puzzle.new setup

remove :: Puzzle -> [Int] -> Puzzle
remove this cellNumbers =
  foldr (\ cellNumber accum ->
          case cellNumber `elem` cellNumbers of
            True -> accum
            False ->
              case Puzzle.getDigit this cellNumber of
                Nothing -> accum
                Just digit -> Puzzle.place accum cellNumber  digit)
        Puzzle.empty
        [0..80]

getDigit :: Puzzle -> Int -> Maybe Int
getDigit this cellNumber =
  Cell.getPlaced $ Cells.getCell (cells this) cellNumber

shuffle :: Random.StdGen -> [a] -> [a]
shuffle rnd list =
  Shuffle.shuffle' list (length list) rnd

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
