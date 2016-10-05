module Step (
  Step (Initial, EasyPeasy),
) where

import Placement (Placement)
import Puzzle (Puzzle)

data Step =
    Initial Puzzle
  | EasyPeasy Puzzle Placement
  deriving (Show)
