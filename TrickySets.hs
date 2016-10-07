module TrickySets (
  TrickySet,
  TrickySets.name,
  TrickySets.common,
  TrickySets.rest,
  TrickySets.eliminate,
  TrickySets.checkNeeded,
  TrickySets.trickySets,
) where

import qualified ExclusionSet
import ExclusionSet (ExclusionSet (ExclusionSet))
import qualified Data.List as List

-- True to check an "inverted" TrickySet, where we scan the coincident
-- rows and columns for needed digits rather than coincident squares.
-- I personally don't do this, it's too hard.
--
checkCoincidentRowsColumnsForNeeded = False

-- XXX Checking the eliminate set for newly forced Unknows isn't implemented.
-- It's also not something I do.
checkForced = False

data TrickySet = TrickySet {
  name :: String,
  common :: [Int],
  rest :: [Int],
  eliminate :: [Int],
  checkNeeded :: [[Int]]
} deriving (Show)

-- Within a square, if the only possible places for a given digit are
-- in the same row/col, then the digit can be removed from the
-- possibilities for the rest of the Unknowns in that row/col.
--
-- The reverse of the situation is also true.  In a given row or
-- column if it is only possible to place a given digit within a
-- single square, then the digit can be eliminated from the other
-- Unknowns of that square.
--
-- Each tuple in trickySets contains three lists of Unknowns.  If a
-- digit is possible in the first list but not the second, it will be
-- removed from the possibiles of the third.
--

-- After we apply a tricky set with a particular digit, it may create
-- some immediate placements:
-- - There may be 

-- - Some of the Unknowns in the eliminate set may now have only one possibility.

trickySets :: [TrickySet]
trickySets =
  let rows = ExclusionSet.rows
      columns = ExclusionSet.columns
      squares = ExclusionSet.squares
      getRows = getCellSetsIncluding rows
      getColumns = getCellSetsIncluding columns
  in concat $
       [createTrickySetsFrom square row getColumns
         | square <- squares, row <- rows] ++
       [createTrickySetsFrom square col getRows
         | square <- squares, col <- columns]

-- XXX Instead of creating two TrickySets with an identical common set, we
-- should merge them into a single set with two branches.

createTrickySetsFrom:: ExclusionSet -> ExclusionSet -> ([Int] -> [[Int]]) -> [TrickySet]
createTrickySetsFrom square row getSetsIncluding =
  let (\\) = (List.\\)
      ExclusionSet squareName squareCells = square
      ExclusionSet rowName rowCells = row
      common = squareCells `List.intersect` rowCells
      restOfRow = rowCells \\ common
      restOfSquare = squareCells \\ common
  in case common of
       [] -> []
       _ ->
         [
           TrickySet {
             name = unwords ["TrickySet", squareName, rowName],
             common = common, rest = restOfSquare, eliminate = restOfRow,
             checkNeeded = getSquaresIncluding restOfRow
           }
         ] ++
         if checkCoincidentRowsColumnsForNeeded
           then [
             TrickySet {
               name = unwords ["Inverse TrickySet", squareName, rowName],
               common = common, rest = restOfRow, eliminate = restOfSquare,
               checkNeeded = getSetsIncluding restOfSquare
             }
           ]
           else []

-- Given some ExclusionSets and some cellNumbers, return the
-- ExclusionSets that contain them.
--
getCellSetsIncluding :: [ExclusionSet] -> [Int] -> [[Int]]
getCellSetsIncluding exclusionSets cells =
  let cellSets = map ExclusionSet.cells exclusionSets
  in filter (not . null . List.intersect cells) cellSets

-- Given some cellNumbers, return the ExclusionSet squares containing
-- them.
--
getSquaresIncluding :: [Int] -> [[Int]]
getSquaresIncluding cells =
  getCellSetsIncluding ExclusionSet.squares cells
