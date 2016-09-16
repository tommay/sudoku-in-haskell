module Unknown
(
  Unknown,
  Unknown.new,
  Unknown.cellNumber,
  Unknown.place,
) where  

import qualified Data.List as List

data Unknown = Unknown {
  cellNumber :: Int,
  row :: Int,
  col :: Int,
  square :: Int,
  possible :: [Int]
} deriving (Show, Eq)

-- Returns a new Unknown at position cellNumber.  Determine the
-- Unknown's row, column, and square, set all digits possible.
--
new :: Int -> Unknown
new cellNumber =
    let row = cellNumber `div` 9
        col = cellNumber `mod` 9
        square = (row `div` 3)*3 + (col `div` 3)
    in Unknown {
      cellNumber = cellNumber,
      row = row,
      col = col,
      square = square,
      possible = [1..9]
    }

place :: Unknown -> Unknown -> Int -> Unknown
place this other digit =
  case isExcludedBy this other of
    True -> this { possible = List.delete digit $ possible this }
    False -> this

-- Returns true if this and Other are in the same row, column, or
-- square, else false.
-- An Unknown does not exclude itself.
--
isExcludedBy :: Unknown -> Unknown -> Bool
isExcludedBy this other =
  this /= other &&
    any (\ f -> f this == f other) [row, col, square]
