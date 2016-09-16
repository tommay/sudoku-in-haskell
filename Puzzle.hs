module Puzzle
(
  Puzzle,
--  Puzzle.empty,
--  Puzzle.remove,
--  Puzzle.solutions,
--  Puzzle.randomSolutions,
--  Puzzle.solutionsFor,
--  Puzzle.toPuzzleString,
) where  

import qualified Data.Char as Char
import qualified Data.List as List
-- import qualified System.Random as Random
-- import qualified System.Random.Shuffle as Shuffle

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
place this unknown digit =
  let cellNumber = Unknown.cellNumber unknown
  in this {
    placed = (Placed.new cellNumber digit : placed this),
    unknown = map (\ u -> Unknown.place u unknown digit)
      $ filter (\ u -> Unknown.cellNumber u /= cellNumber)
      $ Puzzle.unknown this
  }

-- Returns a raw string of 81 digits and dashes, like the argument to
-- new.
--
toString :: Puzzle -> String
toString this =
  map Char.intToDigit $ map Placed.digit $ List.sort $ placed this

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
