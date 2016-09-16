module Cells
(
  Cells,
  Cells.new,
  Cells.adjust,
  Cells.minByPossibleSize,
  Cells.doExclusions,
  Cells.getCell,
  Cells.toString,
) where

import qualified Cell
import Cell (Cell)
import qualified Possible
import Possible (Possible)
import qualified Data.Vector as Vector
import Data.Vector (Vector)

-- Cells holds the state of each Cell, as a tuple containing one Cell
-- for each position on the board, numbered 0 through 80.  This allow
-- us to (somewhat) abstract out the data type Puzzle uses to store
-- Cells.

data Cells = Cells (Vector Cell) deriving (Show)

-- Returns a new Cells with fresh Cells.

new :: Cells
new = Cells $ Vector.generate 81 Cell.new

getCell :: Cells -> Int -> Cell
getCell this cellNumber =
  let Cells vector = this
  in vector Vector.! cellNumber

-- Returns a new Cells with func applied to the indexth element.

adjust :: Cells -> Int -> (Cell -> Cell) -> Cells
adjust this index func =
  let Cells vector = this
      elem = vector Vector.! index
      newElem = func elem
      newVector = vector Vector.// [(index, newElem)]
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
        case Cell.getPossible cell of
          Just possible -> Possible.size possible
          _ -> 10
      Cells vector = this
  in Vector.minimumBy (\a b -> compare (toSize a) (toSize b)) vector

-- Adjust the numbered Cells in ExclusionList to remove Digit from
-- their Possible lists.

doExclusions :: Cells -> Int -> [Int] -> Cells
doExclusions this digit exclusionList =
  foldr
    (\ cellNumber cellsAccum ->
      Cells.adjust
        cellsAccum
        cellNumber
        (\ cell -> Cell.notPossible cell digit))
    this
    exclusionList

toString :: Cells -> String
toString this =
  let Cells vector = this
  in map Cell.toChar $ Vector.toList vector
