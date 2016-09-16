module Puzzle
(
  Puzzle,
--  Puzzle.empty,
--  Puzzle.remove,
--  Puzzle.solutions,
--  Puzzle.randomSolutions,
  Puzzle.solutionsFor,
  Puzzle.toPuzzleString,
) where  

import qualified Data.Char as Char
import qualified Data.List as List
import qualified System.Random as Random
import qualified System.Random.Shuffle as Shuffle

import qualified Placed
import Placed (Placed)
import qualified Unknown
import Unknown (Unknown)

data Puzzle = Puzzle {
  placed :: [Placed],
  unknown :: [Unknown]
} deriving (Show)

-- Returns a new Puzzle with all Unknown cells.
--
empty :: Puzzle
empty =
  Puzzle {
    placed = [],
    unknown = [Unknown.new n | n <- [0..80]]
  }

-- Returns a new Puzzle with each Cell initialized according to
-- Setup, which is a string of 81 digits or dashes.
--
new :: String -> Puzzle
new setup =
  let digits = toDigits setup
      zipped = zip [0..80] digits
  in foldr (\ (cellNumber, digit) this ->
            case digit of
              Nothing -> this
              Just digit -> Puzzle.place this (Unknown.new cellNumber) digit)
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

place :: Puzzle -> Unknown -> Int -> Puzzle
place this unknown' digit =
  let cellNumber = Unknown.cellNumber unknown'
  in this {
    placed = Placed.new cellNumber digit : placed this,
    unknown = map (\ u -> Unknown.place u unknown' digit)
      $ filter (/= unknown')
      $ unknown this
  }

solutionsFor :: String -> [(Int, Puzzle)]
solutionsFor setup =
  solutions $ Puzzle.new setup

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
solutions' this maybeRnd guesses results =
  case unknown this of
    [] ->
      -- No more unknowns, solved!
      (guesses, this) : results
    unknownCells ->
      let minUnknown = Unknown.minByPossibleSize unknownCells
          possible = Unknown.possible minUnknown
      in case possible of
        [] ->
          -- Failed.  No more solutions.
          results
        [_] ->
          -- One possibility.  Recurse without incrementing guesses.
          doGuesses this maybeRnd guesses minUnknown possible results
        _ ->
          -- Multiple possibilities.  Guess each, maybe in a random order,
          -- and recurse.
          let shuffledPossible =
                case maybeRnd of
                  Nothing -> possible
                  Just rnd -> shuffle rnd possible
          in doGuesses this maybeRnd
               (guesses + 1) minUnknown shuffledPossible results

-- For each Digit in the list, use it as a guess for unknown
-- and try to solve the resulting Puzzle.
--
doGuesses :: Puzzle -> Maybe Random.StdGen -> Int -> Unknown -> [Int] -> [(Int, Puzzle)] -> [(Int, Puzzle)]
doGuesses this maybeRnd guesses unknown digits results =
  foldr (\ digit accum ->
          let guess = Puzzle.place this unknown digit
          in solutions' guess maybeRnd guesses accum)
    results
    digits

-- We've got placed and unknown and we have to combine them into s single
-- single lit of characters ordered by originating cellNumber.
-- This can be done by building two lists of (cellNumber, char) then
-- sorting them together and extractin the Chars.
-- Or maybe we could make a Cell type which would be either Placed or Empty,
-- and have them all sort together and have a function that would return
-- their Char.  Maybe.  But for now, this works.
toString this =
  let p = map (\ x -> (Placed.cellNumber x, Char.intToDigit $ Placed.digit x))
            $ placed this
      u = map (\ x -> (Unknown.cellNumber x, '-'))
            $ unknown this
  in map snd $ List.sort $ p ++ u

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

shuffle :: Random.StdGen -> [a] -> [a]
shuffle rnd list =
  Shuffle.shuffle' list (length list) rnd
