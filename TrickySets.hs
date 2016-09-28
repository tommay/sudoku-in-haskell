module TrickySets (
  TrickySets.trickySets,
) where


import qualified ExclusionSets
import qualified Data.List as List

-- Within a square, if the only possible places for a given digit are
-- in the same row/col, then the digit can be removed from the
-- possibilities for the rest of the Unknowns in that row/col.
--
-- The reverse of the situation is also true.  In a given row or
-- column if it is only possible to place a given digit within a
-- single square, then the digit can be eliminated from the other
-- Unknowns of that square.
--
-- Each tuple in trickSets contains three lists of Unknowns.  If a
-- digit is possible in the first list but not the second, it will be
-- removed from the possibiles of the third.
-- XXX This should be changed to [([Int], [([Int], [Int])])].
--
trickySets :: [([Int], [Int], [Int])]
trickySets =
  let (rows, cols, squares) = ExclusionSets.exclusionSetsTuple
      product = cartesianProduct (rows ++ cols) squares
      (\\) = (List.\\)
  in concat $
       map (\ (row, square) ->
             case row `List.intersect` square of
               [] -> []
               common ->
                 [
                   (common, square \\ common, row \\ common),
                   (common, row \\ common, square \\ common)
                 ])
         product

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct as bs =
  [(a, b) | a <- as, b <- bs]

