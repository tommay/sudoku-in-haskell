module ExclusionSets (
  ExclusionSets.exclusionSets,
  ExclusionSets.squares,
  ExclusionSets.exclusionSetsTuple,  -- XXX until TrickySets is split out of Puzzle.
) where

-- An ExclusionSet is a list of all the cell numbers in a row, column,
-- or square.  They're used as part of the heuristic methods in Solver.
--
exclusionSets :: [[Int]]
exclusionSets =
  let (rows, cols, squares) = exclusionSetsTuple
  in rows ++ cols ++ squares

-- Create an ExclusionSet for each square.
--
squares :: [[Int]]
squares = map (\ square ->
    let
      -- row and col of upper left corner of square
      row = square `div` 3 * 3
      col = square `mod` 3 * 3
    in
      [(row + n `div` 3)*9 + (col + n `mod` 3) | n <- [0..8]])
  [0..8]

exclusionSetsTuple :: ([[Int]], [[Int]], [[Int]])
exclusionSetsTuple =
 let
   -- Create an ExclusionSet for each row, containing the cell numbers
   -- in the row.
   rows = [[row*9 + col | col <- [0..8]] | row <- [0..8]]

   -- Create an ExclusionSet for each column.
   cols = [[row*9 + col | row <- [0..8]] | col <- [0..8]]

  in (rows, cols, squares)
