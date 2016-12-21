module Puzzle (
  Puzzle,
  Puzzle.empty,
  Puzzle.size,
  Puzzle.each,
  Puzzle.unknownCellNumbers,
  Puzzle.fromString,
  Puzzle.place,
  Puzzle.remove,
  Puzzle.toPuzzleString,
) where  

import           Digit (Digit)
import qualified Util

import qualified Data.Char as Char
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set

-- cellNumber -> Digit

data Puzzle = Puzzle {
  placed :: Map Int Digit
} deriving (Show)

-- Returns a new Puzzle with nothing placed.
--
empty :: Puzzle
empty =
  Puzzle.Puzzle {
    placed = Map.empty
  }

each :: Puzzle -> [(Int, Digit)]
each this =
  Map.assocs $ Puzzle.placed this

cellNumbers :: Puzzle -> [Int]
cellNumbers this =
  Map.keys $ Puzzle.placed this

allUnknownCellNumbers :: Set Int
allUnknownCellNumbers = Set.fromList [0..80]

unknownCellNumbers :: Puzzle -> [Int]
unknownCellNumbers this =
  Set.toList $ foldr Set.delete allUnknownNumbers $ Puzzle.cellNumbers this

place :: Puzzle -> Int -> Digit -> Puzzle
place this cellNumber digit =
  this {
    placed = Map.insert cellNumber digit $ Puzzle.placed this
  }

remove :: Puzzle -> [Int] -> Puzzle
remove this cellNumbers =
  let notInCellNumbers key _ = not $ key `elem` cellNumbers
      remaining = Map.filterWithKey notInCellNumbers $ Puzzle.placed this
  in this { placed = remaining }

-- Returns the number of placed digits.
--
size :: Puzzle -> Int
size this =
  Map.size $ Puzzle.placed this

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
  let p = map (\ (k, v) -> (k, Char.intToDigit v)) $ each this
      u = zip (unknownCellNumbers this) $ repeat '-'
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
