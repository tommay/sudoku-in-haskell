module ExclusionSet (
  ExclusionSet (ExclusionSet),
  exclusionSets,
  row,
  rows,
  column,
  columns,
  squares,
  name,
  cells,
) where

import           Data.Set (Set)
import qualified Data.Set as Set

data ExclusionSet = ExclusionSet {
  name :: String,
  cells :: [Int],
  cellSet :: Set Int
} deriving (Show, Eq)

-- An ExclusionSet is a list of all the cell numbers in a row, column,
-- or square.  They're used as part of the heuristic methods in Solver.
--
exclusionSets :: [ExclusionSet]
exclusionSets =
  rows ++ columns ++ squares

make :: String -> [Int] -> ExclusionSet
make name list =
  ExclusionSet.ExclusionSet name list $ Set.fromList list

-- An ExclusionSet for each row.
--
rows :: [ExclusionSet]
rows = map row [0..8]

row :: Int -> ExclusionSet
row n =
  make ("row " ++ show n) [n*9 + col | col <- [0..8]]

-- An ExclusionSet for each column.
--
columns :: [ExclusionSet]
columns = map column [0..8]

column :: Int -> ExclusionSet
column n =
  make ("column " ++ show n) [row*9 + n | row <- [0..8]]

-- An ExclusionSet for each square.
--
squares :: [ExclusionSet]
squares = map square [0..8]

square :: Int -> ExclusionSet
square n =
  let -- row and col of upper left corner of square
      row = n `div` 3 * 3
      col = n `mod` 3 * 3
      cellNumbers = [(row + n `div` 3)*9 + (col + n `mod` 3) | n <- [0..8]]
  in make ("square " ++ show n) cellNumbers
