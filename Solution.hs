module Solution
(
  Solution,
  new,
  puzzle,
  stats,
) where

import Puzzle (Puzzle)
import Stats (Stats)

data Solution = Solution {
  puzzle :: Puzzle,
  stats :: Stats
} deriving (Show)

new :: Puzzle -> Stats -> Solution
new puzzle stats =
  Solution { puzzle = puzzle, stats = stats }
