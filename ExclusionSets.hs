module ExclusionSets (
  ExclusionSets.exclusionSets,
  ExclusionSets.rows,
  ExclusionSets.columns,
  ExclusionSets.squares,
) where

-- An ExclusionSet is a list of all the cell numbers in a row, column,
-- or square.  They're used as part of the heuristic methods in Solver.
--
exclusionSets :: [[Int]]
exclusionSets =
  rows ++ columns ++ squares

-- An ExclusionSet for each row.
--
rows :: [[Int]]
rows = [row n | n <-[0..8]]

row :: Int -> [Int]
row n = [n*9 + col | col <- [0..8]]

-- An ExclusionSet for each column.
--
columns :: [[Int]]
columns = [column n | n <- [0..8]]

column :: Int -> [Int]
column n = [row*9 + n | row <- [0..8]]

-- An ExclusionSet for each square.
--
squares :: [[Int]]
squares =
  map (\ square ->
        let
          -- row and col of upper left corner of square
          row = square `div` 3 * 3
          col = square `mod` 3 * 3
        in
          [(row + n `div` 3)*9 + (col + n `mod` 3) | n <- [0..8]])
    [0..8]
