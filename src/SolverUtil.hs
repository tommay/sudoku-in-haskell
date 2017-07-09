module SolverUtil (
  unknownsInSet,
  isUnknownInSet,
) where

import qualified Unknown
import           Unknown (Unknown)

unknownsInSet :: [Unknown] -> [Int] -> [Unknown]
unknownsInSet unknowns set =
  filter (isUnknownInSet set) unknowns

isUnknownInSet :: [Int] -> Unknown -> Bool
isUnknownInSet list unknown =
  Unknown.cellNumber unknown `elem` list
