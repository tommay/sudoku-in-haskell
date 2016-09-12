module Cells
( Cells
, Cells.new  
, Cells.update
, Cells.minByPossibleSize
, Cells.doExclusions
, Cells.toString
) where

import qualified Cell
import Cell (Cell)
import qualified Possible
import Possible (Possible)
import qualified Data.Sequence as Sequence
import Data.Sequence (Seq)
import qualified Data.Foldable as Foldable

-- Cells holds the state of each Cell, as a tuple containing one Cell
-- for each position on the board, numbered 0 through 80.  This allow
-- us to (somewhat) abstract out the data type Puzzle uses to store
-- Cells.

data Cells = Cells (Seq Cell) deriving (Show)

-- Returns a new Cells with fresh Cells.

new :: Cells
new = Cells $ Sequence.fromFunction 81 Cell.new

-- Returns a new Cells with func applied to the indexth element.

update :: Cells -> Int -> (Cell -> Cell) -> Cells
update this index func =
  let Cells seq = this
      newSeq = Sequence.adjust func index seq
  in Cells newSeq

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
      Cells seq = this
  in Foldable.minimumBy (\a b -> compare (toSize a) (toSize b)) seq

-- Update the numbered Cells in ExclusionList to remove Digit from
-- their Possible lists.

doExclusions :: Cells -> Int -> [Int] -> Cells
doExclusions this digit exclusionList =
  foldr
    (\ cellNumber cellsAccum ->
      Cells.update
        cellsAccum
        cellNumber
        (\ cell -> Cell.notPossible cell digit))
    this
    exclusionList

toString :: Cells -> String
toString this =
  let Cells seq = this
  in map Cell.toChar $ Foldable.toList seq
