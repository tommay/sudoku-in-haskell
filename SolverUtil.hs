module SolverUtil (
  unknownsInSet,
  isUnknownInSet,
) where

import qualified Puzzle
import           Puzzle (Puzzle)
import qualified Unknown
import           Unknown (Unknown)

unknownsInSet :: Puzzle -> [Int] -> [Unknown]
unknownsInSet puzzle set =
  filter (isUnknownInSet set) $ Puzzle.unknown puzzle

isUnknownInSet :: [Int] -> Unknown -> Bool
isUnknownInSet list unknown =
  Unknown.cellNumber unknown `elem` list
