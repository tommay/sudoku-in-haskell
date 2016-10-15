module Unknown (
  Unknown,
  Unknown.new,
  Unknown.cellNumber,
  Unknown.possible,
  Unknown.place,
  Unknown.removeDigitFromPossible,
) where  

import Digit (Digit)

import qualified Data.List as List

data Unknown = Unknown {
  cellNumber :: Int,
  row :: Int,
  col :: Int,
  square :: Int,
  possible :: [Digit]
} deriving (Show)

-- Check for equality by testing cellNumber and possible.  The other fields
-- are functions of cellNumber.
--
instance Eq Unknown where
  this == that = 
    (cellNumber this == cellNumber that) && (possible this == possible that)

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

place :: Int -> Digit -> Unknown -> Unknown
place cellNumber digit this =
  let other = Unknown.new cellNumber
  in if isExcludedBy this other
       then removeDigitFromPossible digit this
       else this

removeDigitFromPossible :: Digit -> Unknown -> Unknown
removeDigitFromPossible digit this =
  this { possible = List.delete digit $ possible this }

-- Returns true if this and Other are in the same row, column, or
-- square, else false.
-- An Unknown does not exclude itself.  I'm not sure we actually
-- have to check for this in practice, but better safe than sorry.
--
isExcludedBy :: Unknown -> Unknown -> Bool
isExcludedBy this other =
  (Unknown.cellNumber this /= Unknown.cellNumber other) &&
    any (\ f -> f this == f other) [row, col, square]
