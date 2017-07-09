module Solution (
  Solution,
  new,
  puzzle,
  steps
) where

import Puzzle (Puzzle)
import Step (Step)

data Solution = Solution {
  puzzle :: Puzzle,
  steps :: [Step]
} deriving (Show)

new :: Puzzle -> [Step] -> Solution
new puzzle steps =
  Solution { puzzle = puzzle, steps = steps }
