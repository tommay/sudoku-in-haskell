module Solution (
  Solution,
  new,
  puzzle,
  steps,
  stats,
) where

import Puzzle (Puzzle)
import Step (Step)
import Stats (Stats)

data Solution = Solution {
  puzzle :: Puzzle,
  steps :: [Step],
  stats :: Stats
} deriving (Show)

new :: Puzzle -> [Step] -> Stats -> Solution
new puzzle steps stats =
  Solution { puzzle = puzzle, steps = steps, stats = stats }
