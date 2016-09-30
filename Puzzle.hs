module Puzzle (
  Puzzle,
  Puzzle.unknown,
  Puzzle.empty,
  Puzzle.fromString,
  Puzzle.place,
  Puzzle.remove,
  Puzzle.notPossibleForList,
  Puzzle.toPuzzleString,
) where  

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Debug.Trace

import qualified Placed
import Placed (Placed)
import qualified Unknown
import Unknown (Unknown)
import qualified ExclusionSets

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
fromString :: String -> Puzzle
fromString setup =
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
      $ filter (\ x -> Unknown.cellNumber x /= cellNumber)
      $ unknown this
  }

remove :: Puzzle -> [Int] -> Puzzle
remove this cellNumbers =
  let remaining = filter (\p -> not $ Placed.cellNumber p `elem` cellNumbers)
        $ placed this
  in foldr (\p accum ->
             Puzzle.place accum
               (Unknown.new $ Placed.cellNumber p)
               (Placed.digit p))
       Puzzle.empty
       remaining

-- Some external code has done some mojo and etermined that it's not actually
-- possible to put the digit into any cells in the list.  Fix up unknowns
-- to reflect this.
--
notPossibleForList :: Puzzle -> Int -> [Int] -> Puzzle
notPossibleForList this digit cellNumbers =
  this {
    unknown = map
      (\ u ->
        if Unknown.cellNumber u `elem` cellNumbers
           then Unknown.removeDigitFromPossible digit u
           else u)
      $ unknown this
  }

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
-- anything fancy.
--
slices :: Int -> [a] -> [[a]]
slices _ [] = []
slices n list =
  let (slice, rest) = splitAt n list
  in slice : slices n rest
