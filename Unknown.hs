module Unknown (
  Unknown,
  Unknown.new,
  Unknown.cellNumber,
  Unknown.place,
  Unknown.numPossible,
  Unknown.isDigitPossible,
  Unknown.removeDigitFromPossible,
  Unknown.getPossible
) where  

import Digit (Digit)

import qualified Data.Bits as Bits

data Unknown = Unknown {
  cellNumber :: Int,
  row :: Int,
  col :: Int,
  square :: Int,
  possible :: Int
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
    possible = 0x1FF
  }

-- XXX Check that the bit is set first?
place :: Int -> Digit -> Unknown -> Unknown
place cellNumber digit this =
  let other = Unknown.new cellNumber
  in if isExcludedBy this other
       then removeDigitFromPossible digit this
       else this

numPossible :: Unknown -> Int
numPossible this =
  Bits.popCount $ possible this

isDigitPossible :: Digit -> Unknown -> Bool
isDigitPossible digit this =
  Bits.testBit (possible this) (digit - 1)

removeDigitFromPossible :: Digit -> Unknown -> Unknown
removeDigitFromPossible digit this =
  this { possible = Bits.clearBit (possible this) (digit - 1) }

getPossible :: Unknown -> [Digit]
getPossible this =
  getPossibleList (possible this) 1

getPossibleList :: Int -> Digit -> [Digit]
getPossibleList 0 _ =
  []
getPossibleList possible digit =
  if Bits.testBit possible 0
    then digit : getPossibleList (Bits.shiftR possible 1) (digit + 1)
    else getPossibleList (Bits.shiftR possible 1) (digit + 1)

-- Returns true if this and Other are in the same row, column, or
-- square, else false.
-- An Unknown does not exclude itself.  I'm not sure we actually
-- have to check for this in practice, but better safe than sorry.
--
isExcludedBy :: Unknown -> Unknown -> Bool
isExcludedBy this other =
  (Unknown.cellNumber this /= Unknown.cellNumber other) &&
    any (\ f -> f this == f other) [row, col, square]
