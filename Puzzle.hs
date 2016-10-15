module Puzzle (
  Puzzle,
  Puzzle.empty,
  Puzzle.placed,
  Puzzle.size,
  Puzzle.fromString,
  Puzzle.place,
  Puzzle.remove,
  Puzzle.toPuzzleString,
) where  

import           Digit (Digit)
import qualified Placed
import           Placed (Placed)
import qualified Util

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Debug.Trace

data Puzzle = Puzzle {
  placed :: [Placed]
} deriving (Show)

-- Returns a new Puzzle with nothing placed.
--
empty :: Puzzle
empty =
  Puzzle {
    placed = []
  }

place :: Puzzle -> Int -> Digit -> Puzzle
place this cellNumber digit =
  this {
    placed = Placed.new cellNumber digit : placed this
  }

remove :: Puzzle -> [Int] -> Puzzle
remove this cellNumbers =
  let remaining = filter (not . (`elem` cellNumbers) . Placed.cellNumber)
        $ placed this
  in this{ placed = remaining }

-- Returns the number of placed digits.
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
              Just digit -> Puzzle.place this cellNumber digit)
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

-- The opposite of fromString.  Given a Puzzle, create a string of 81
-- digits or dashes.  Creates two lists of (cellNumber, Char), one for
-- placed cells and one for unplaced cells, then sorts them together and
-- extracts the Chars in order.
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
