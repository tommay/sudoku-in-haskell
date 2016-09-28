module TrickySets (
  TrickySet,
  TrickySets.common,
  TrickySets.rest,
  TrickySets.eliminate,
  TrickySets.checkNeeded,
  TrickySets.trickySets,
) where

import qualified ExclusionSets
import qualified Data.List as List

data TrickySet = TrickySet {
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
  let rows = ExclusionSets.rows
      columns = ExclusionSets.columns
      squares = ExclusionSets.squares
  in concat $
       [createTrickySetsFrom square row | square <- squares, row <- rows] ++
       [createTrickySetsFrom square col | square <- squares, col <- columns]

createTrickySetsFrom:: [Int] -> [Int] -> [TrickySet]
createTrickySetsFrom square row =
  let (\\) = (List.\\)
      common = row `List.intersect` square
      restOfRow = row \\ common
  in case common of
       [] -> []
       _ ->
         [
           TrickySet {
             common = common, rest = square \\ common, eliminate = restOfRow,
             checkNeeded = getSquaresIncluding restOfRow
           },
           TrickySet {
             common = common, rest = restOfRow, eliminate = square \\ common,
             checkNeeded = []
           }
         ]

-- Given some cellNumbers, return the ExclusionSet squares containing
-- them.
--
getSquaresIncluding :: [Int] -> [[Int]]
getSquaresIncluding cells =
  filter (\ square ->
           case square `List.intersect` cells of
             [] -> False
             _ -> True)
    ExclusionSets.squares
