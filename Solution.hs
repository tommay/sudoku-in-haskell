module Solution
(
  Solution,
  new,
  guessCount,
  puzzle,
) where

import Puzzle (Puzzle)

data Solution = Solution {
  guessCount :: Int,
  puzzle :: Puzzle
} deriving (Show)

new :: Int -> Puzzle -> Solution
new guessCount puzzle =
  Solution { guessCount = guessCount, puzzle = puzzle }
