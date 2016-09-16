module Placed
(
  Placed,
  Placed.new,
  Placed.cellNumber,
  Placed.digit,
) where  

data Placed = Placed {
  cellNumber :: Int,
  digit :: Int
} deriving (Show)

new :: Int -> Int -> Placed
new cellNumber digit =
  Placed {
    cellNumber = cellNumber,
    digit = digit
  }
