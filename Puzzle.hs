module Puzzle (
  Puzzle,
  Puzzle.unknown,
  Puzzle.empty,
  Puzzle.placed,
  Puzzle.size,
  Puzzle.fromString,
  Puzzle.place,
  Puzzle.remove,
  Puzzle.notPossibleForList,
  Puzzle.toPuzzleString,
) where  

import           Digit (Digit)
import qualified Placed
import           Placed (Placed)
import qualified Unknown
import           Unknown (Unknown)
import qualified Util

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Debug.Trace



data Puzzle = Puzzle {
  placed :: [Placed],
  unknown :: [Unknown]
} deriving (Show, Eq)

-- Returns a new Puzzle with all Unknown cells.
--
empty :: Puzzle
empty =
  Puzzle {
    placed = [],
    unknown = [Unknown.new n | n <- [0..80]]
  }

-- Returns the number of places digits.
--
size :: Puzzle -> Int
size this =
  length $ Puzzle.placed this

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

-- Given a Setup string, returns a list of Maybe Digit for
-- each cell.
--
toDigits :: String -> [Maybe Digit]
toDigits setup =
  [case char of
    '-' -> Nothing
    _ -> Just $ Char.digitToInt char
   | char <- setup]

place :: Puzzle -> Unknown -> Digit -> Puzzle
place this unknown' digit =
  let cellNumber = Unknown.cellNumber unknown'
  in this {
    placed = Placed.new cellNumber digit : placed this,
    unknown = map (\ u -> Unknown.place u unknown' digit)
      $ filter ((/= cellNumber) . Unknown.cellNumber)
      $ unknown this
  }

remove :: Puzzle -> [Int] -> Puzzle
remove this cellNumbers =
  let remaining = filter (not . (`elem` cellNumbers) . Placed.cellNumber)
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
notPossibleForList :: Puzzle -> Digit -> [Int] -> Puzzle
notPossibleForList this digit cellNumbers =
  this {
    unknown = map
      (\ u ->
        if Unknown.cellNumber u `elem` cellNumbers
           then Unknown.removeDigitFromPossible digit u
           else u)
      $ unknown this
  }

-- The opposite of fromString.  Given a Puzzle, create a string of 81
-- digits or dashes.  Creates two lists of (cellNumber, Char), one for
-- placed cells and one for unplaced cells, then sorts them together and
-- exracts the Chars in order.
--
toString:: Puzzle -> String
toString this =
  let p = map (\ x -> (Placed.cellNumber x, Char.intToDigit $ Placed.digit x))
            $ placed this
      unknownNumbers = (List.\\) [0..80] $ map Placed.cellNumber $ placed this
      u = zip unknownNumbers $ repeat '-'
  in map snd $ List.sort $ p ++ u

-- Returns a string that prints out as a grid of digits.
--
toPuzzleString :: Puzzle -> String
toPuzzleString this =
  unlines $
    map (\puzzles ->
          unlines $
            map (\row -> unwords $ Util.slices 3 row) $
              Util.slices 9 puzzles) $
      Util.slices 27 $ Puzzle.toString this
