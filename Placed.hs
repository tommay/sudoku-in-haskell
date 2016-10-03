module Placed
(
  Placed,
  Placed.new,
  Placed.cellNumber,
  Placed.digit,
) where  

import Digit (Digit)

data Placed = Placed {
  cellNumber :: Int,
  digit :: Digit
} deriving (Show)

new :: Int -> Digit -> Placed
new cellNumber digit =
  Placed {
    cellNumber = cellNumber,
    digit = digit
  }
