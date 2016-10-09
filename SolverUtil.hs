module SolverUtil (
  unknownsInSet,
  isUnknownInSet,
) where

import Puzzle (Puzzle)
import qualified Puzzle
import Unknown (Unknown)
import qualified Unknown

unknownsInSet :: Puzzle -> [Int] -> [Unknown]
unknownsInSet puzzle set =
  filter (isUnknownInSet set) $ Puzzle.unknown puzzle

isUnknownInSet :: [Int] -> Unknown -> Bool
isUnknownInSet list unknown =
  Unknown.cellNumber unknown `elem` list
