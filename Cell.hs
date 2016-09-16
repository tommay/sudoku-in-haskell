module Cell
(
  Cell,
  Cell.new,
  getNumber,
  getPossible,
  place,
  getPlaced,
  isExcludedBy,
  notPossible,
  toChar,
) where  

import qualified Data.Char as Char
import qualified Possible
import Possible (Possible)

-- A Cell records the state of an individual sudoku square.
-- They're not called "squares" because that's used for the squares
-- that contain nine Cells.  A Cell knows:
-- - its number from 0 to 80.  Cells are numbered across the board
--   from left to right and top to bottom.
-- - its row.  Rows are numbered 0 to 8 from top to bottom.
-- - its column.  Rows are numbered 0 to 8 from left to right.
-- - its square.  Squares are numbered across the board
--   from left to right and top to bottom.
-- - which digits are Possible to place in this Cell, i.e., which
--   digits don't conflict with previously placed digits in other
--   Cells in the same row/column/square.
-- - which digit has been placed in this Cell.

data Cell = Cell {
  number :: Int,
  row :: Int,
  col :: Int,
  square :: Int,
  value :: Value
} deriving (Show)

data Value = Placed Int | NotPlaced Possible deriving (Show)

-- Returns a new Cell at position Number.  Determine the Cell's row,
-- column, and square, create a new Possible with all digits possible,
--
new :: Int -> Cell
new number =
    let row = number `div` 9
        col = number `mod` 9
        square = (row `div` 3)*3 + (col `div` 3)
    in Cell {
      number = number,
      row = row,
      col = col,
      square = square,
      value = NotPlaced Possible.new
    }

getNumber :: Cell -> Int
getNumber = number

getPlaced :: Cell -> Maybe Int
getPlaced this =
  case value this of
    Placed number -> Just number
    NotPlaced _ -> Nothing

getPossible :: Cell -> Maybe Possible
getPossible this =
  case value this of
    Placed _ -> Nothing
    NotPlaced possible -> Just possible

place :: Cell -> Int -> Cell
place this digit = this{value = Placed digit}

-- Returns true if This and Other are in the same row, column, or
-- square, else false.
-- A Cell does not exclude itself.
--
isExcludedBy :: Cell -> Cell -> Bool
isExcludedBy this other =
  number this /= number other &&
    any (\ f -> f this == f other) [row, col, square]

-- Returns a new Cell with Digit removed from the Possible set.
--
notPossible :: Cell -> Int -> Cell
notPossible this digit =
  case value this of
    Placed _ -> this
    NotPlaced possible ->
      this{value = NotPlaced $ Possible.remove possible digit}

toChar :: Cell -> Char
toChar this =
  case Cell.getPlaced this of
    Nothing -> '-'
    Just digit -> Char.intToDigit digit
