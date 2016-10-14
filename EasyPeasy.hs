module EasyPeasy (
  find,
) where

import           Digit (Digit)
import qualified ExclusionSet
import           ExclusionSet (ExclusionSet)
import qualified Next
import           Next (Next)
import qualified Placed
import           Placement (Placement (Placement))
import qualified Puzzle as Puzzle
import           Puzzle (Puzzle)
import qualified SolverUtil
import qualified Unknown
import qualified Util

import qualified Data.List as List

-- Easy peasies are found by using stripes of three rows or columns.
-- The first item in the tuples is the set we're looking to place a
-- digit in.  Thw other two are the other two rows/columns in the
-- stripe.  Here we build all the possible stripes so they can be
-- searched for easy peasies.
--
makeEasyPeasyStripes :: [(ExclusionSet, [ExclusionSet])]
makeEasyPeasyStripes =
  concat $ map makeEasyPeasyStripe
  $ Util.slices 3 (ExclusionSet.rows ++ ExclusionSet.columns)

makeEasyPeasyStripe :: [ExclusionSet] -> [(ExclusionSet, [ExclusionSet])]
makeEasyPeasyStripe slice =
  map (\ set -> (set, List.delete set slice)) slice

-- Return a list of all possible easy peasy placements for the Puzzle.
--
find :: Puzzle -> [Next]
find puzzle =
  concat $ map (findForEasyPeasyStripe puzzle) $ makeEasyPeasyStripes

-- Returns any easy peasies in the Puzzle and EasyPeasyStripe.  All
-- digits are considered
--
findForEasyPeasyStripe :: Puzzle -> (ExclusionSet, [ExclusionSet]) -> [Next]
findForEasyPeasyStripe puzzle (col0, [col1, col2]) =
  let digitsInCol1 = getDigitsInSet puzzle col1
      digitsInCol2 = getDigitsInSet puzzle col2
      easyPeasyDigits = (digitsInCol1 `List.intersect` digitsInCol2)
  in concat $ map (placeDigitInSet puzzle col0) easyPeasyDigits

getDigitsInSet :: Puzzle -> ExclusionSet -> [Digit]
getDigitsInSet puzzle set =
  map Placed.digit
  $ filter (\ p -> Placed.cellNumber p `elem` ExclusionSet.cells set)
  $ Puzzle.placed puzzle

placeDigitInSet :: Puzzle -> ExclusionSet -> Digit -> [Next]
placeDigitInSet puzzle set digit =
  let unknowns = SolverUtil.unknownsInSet puzzle $ ExclusionSet.cells set
  in case filter (elem digit . Unknown.possible) unknowns of
      [unknown] -> [Next.new ("Easy peasy " ++ ExclusionSet.name set)
                    id digit unknown]
      _ -> []

