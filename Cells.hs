module Cells
( Cells
, Cells.new  
, Cells.update
, Cells.minByPossibleSize
, Cells.doExclusions
) where

import Cell
import Possible
import Data.Vector as Vector

-- Cells holds the state of each Cell, as a tuple containing one Cell
-- for each position on the board, numbered 0 through 80.  This allow
-- us to (somewhat) abstract out the data type Puzzle uses to store
-- Cells.

data Cells = Cells (Vector Cell) deriving (Show)

-- Returns a new Cells with fresh Cells.

new :: Cells
new = Cells $ Vector.generate 81 Cell.new

-- Returns a new Cells with func applied to the indexth element.

update :: Cells -> Int -> (Cell -> Cell) -> Cells
update this index func =
  let Cells vector = this
      elem = vector ! index
      newElem = func elem
      newVector = vector // [(index, newElem)]
  in Cells newVector

-- Returns the unplaced Cell with the smallest set of possibilities.
-- This is used to find the best Cell to make a guess for to minimize
-- the amount of guessing.
-- XXX This is super-ugly and has to evaluate the toSize function too
-- much.  Is there anything like ruby's min_by?

minByPossibleSize :: Cells -> Cell
minByPossibleSize this =
  let toSize cell =
        -- Sort placed Cells to the end.
        case getPossible cell of
          Just possible -> Possible.size possible
          _ -> 10
      Cells vector = this
  in Vector.minimumBy (\a b -> compare (toSize a) (toSize b)) vector

-- Update the numbered Cells in ExclusionList to remove Digit from
-- their Possible lists.

doExclusions :: Cells -> Int -> [Int] -> Cells
doExclusions this digit exclusionList =
  Prelude.foldr
    (\ cellNumber cellsAccum ->
      Cells.update
        cellsAccum
        cellNumber
        (\ cell -> Cell.notPossible cell digit))
    this
    exclusionList
