module Step (
  Step (Step),
) where

import Placement (Placement)
import Puzzle (Puzzle)

data Step = Step Puzzle (Maybe Placement) String
  deriving (Show)
