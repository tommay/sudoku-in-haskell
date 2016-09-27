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
-- XXX This should be changed to [([Int], [([Int], [Int])])].
--

-- After we apply a tricky set with a particular digit, it may create
-- some immediate placements:
-- - There may be 

-- - Some of the Unknown in the eliminate set may now have ony one possibility.

trickySets :: [TrickySet]
trickySets =
  let (rows, cols, squares) = ExclusionSets.exclusionSetsTuple
      product = cartesianProduct (rows ++ cols) squares
  in concat $
       map (\ (row, square) ->
             case row `List.intersect` square of
               [] -> []
               common -> createTrickySetsFrom common square row)
         product

createTrickySetsFrom:: [Int] -> [Int] -> [Int] -> [TrickySet]
createTrickySetsFrom common square row =
  let (\\) = (List.\\)
      restOfRow = row \\ common
  in [
       TrickySet {
         common = common, rest = square \\ common, eliminate = restOfRow,
         checkNeeded = getSquaresIncluding(restOfRow)
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

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct as bs =
  [(a, b) | a <- as, b <- bs]

