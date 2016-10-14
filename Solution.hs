module Solution (
  Solution,
  new,
  puzzle,
  steps,
  stats,
) where

import Puzzle (Puzzle)
import Stats (Stats)
import Step (Step)

data Solution = Solution {
  puzzle :: Puzzle,
  steps :: [Step],
  stats :: Stats
} deriving (Show)

new :: Puzzle -> [Step] -> Stats -> Solution
new puzzle steps stats =
  Solution { puzzle = puzzle, steps = steps, stats = stats }
