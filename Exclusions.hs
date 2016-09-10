module Exclusions
(Exclusions, new, getListForCell) where

import Data.Vector as Vector
import Data.List as List

data Exclusions = Exclusions (Vector [Int]) deriving (Show)

-- Returns a Vector mapping Number to a list of all cell numbers
-- excluded by that cell number, e.g., 0 => [0, 1, 2, ...].

new :: Exclusions
new =
  let exclusionLists = createExclusionLists
  in Exclusions $
       Vector.generate 81 (getExclusionListForCell exclusionLists)

-- Returns a list for each row, column, and square listing the
-- cells it contains: [[0, 1, 2, ...], [0, 9, 18, ...], ...]
--
createExclusionLists :: [[Int]]
createExclusionLists =
  let -- Create a List for each row, containing the number of each
      -- cell in the row.
      -- XXX USe Exclusion objects with names?

      rows = [[row*9 + col | col <- [0..8]] | row <- [0..8]]

      -- Same for columns.

      cols = [[row*9 + col | row <- [0..8]] | col <- [0..8]]

      -- Same for squarea.

      squares =
        Prelude.map
         (\ square ->
           -- Row and Col of upper left corner of square.
           let row = square `div` 3 * 3
               col = square `mod` 3 * 3
           in [(row + n `div` 3)*9 + (col + n `mod` 3) | n <- [0..8]])
         [0..8]

    in rows Prelude.++ cols Prelude.++ squares

-- Used during initialization.
--
getExclusionListForCell :: [[Int]] -> Int -> [Int]
getExclusionListForCell exclusionLists cellNumber =
  let forCell = Prelude.filter (Prelude.elem cellNumber) exclusionLists
      flattened = Prelude.concat forCell
      filtered = Prelude.filter (/= cellNumber) flattened
  in List.nub filtered

-- Used by place.
--
getListForCell :: Exclusions -> Int -> [Int]
getListForCell this cellNumber =
  let Exclusions vector = this
  in vector ! cellNumber
