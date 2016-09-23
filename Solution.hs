module Solution
(
  Solution (Solution),
  GuessCount (GuessCount),
  guessCount,
  puzzle,
) where

import Puzzle (Puzzle)

data Solution = Solution {
  guessCount :: GuessCount,
  puzzle :: Puzzle
} deriving (Show)

data GuessCount = GuessCount Int deriving (Show)

instance Enum GuessCount where
  toEnum n = GuessCount n
  fromEnum (GuessCount n) = n
