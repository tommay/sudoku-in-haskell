module Puzzle (
  Puzzle,
  Puzzle.unknown,
  Puzzle.exclusionSets,
  Puzzle.trickySets,
  Puzzle.empty,
  Puzzle.fromString,
  Puzzle.place,
  Puzzle.remove,
  Puzzle.notPossibleForList,
  Puzzle.toPuzzleString,
) where  

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Debug.Trace

import qualified Placed
import Placed (Placed)
import qualified Unknown
import Unknown (Unknown)

data Puzzle = Puzzle {
  placed :: [Placed],
  unknown :: [Unknown],
  -- XXX exclusionSets and trickySets should be moved to Solver since
  -- they're only here for Solver's heuristic methods.
  exclusionSets :: [[Int]],
  trickySets :: [([Int], [Int], [Int])]
} deriving (Show)

-- Returns a new Puzzle with all Unknown cells.
--
empty :: Puzzle
empty =
  Puzzle {
    placed = [],
    unknown = [Unknown.new n | n <- [0..80]],
    exclusionSets = makeExclusionSets,
    trickySets = makeTrickySets
  }

-- An ExclusionSet is a list of all the cell numbers in a row, column,
-- or square.  They're used as part of the heuristic methods in Solver.
--
makeExclusionSets :: [[Int]]
makeExclusionSets =
  let (rows, cols, squares) = makeExclusionSetsTuple
  in rows ++ cols ++ squares

makeExclusionSetsTuple :: ([[Int]], [[Int]], [[Int]])
makeExclusionSetsTuple =
 let
   -- Create an ExclusionSet for each row, containing the cell numbers
   -- in the row.
   rows = [[row*9 + col | col <- [0..8]] | row <- [0..8]]

   -- Create an ExclusionSet for each column.
   cols = [[row*9 + col | row <- [0..8]] | col <- [0..8]]

   -- Create an ExclusionSet for each square.
   squares = map (\ square ->
       let
         -- row and col of upper left corner of square
         row = square `div` 3 * 3
         col = square `mod` 3 * 3
       in
         [(row + n `div` 3)*9 + (col + n `mod` 3) | n <- [0..8]])
     [0..8]

  in (rows, cols, squares)

-- Within a square, if the only possible places for a given digit are
-- in the same row/col, then the digit can be removed from the
-- possibilities for the rest of the Unknowns in that row/col.
--
-- The reverse of the situation is also true.  In a given row or
-- column if it is only possible to place a given digit within a
-- single square, then the digit can be eliminated from the other
-- Unknowns of that square.
--
-- Each tuple in trickSets contains three lists of Unknowns.  If a
-- digit is possible in the first list but not the second, it will be
-- removed from the possibiles of the third.
-- XXX This should be changed to [([Int], [([Int], [Int])])].
--
makeTrickySets :: [([Int], [Int], [Int])]
makeTrickySets =
  let (rows, cols, squares) = makeExclusionSetsTuple
      product = cartesianProduct (rows ++ cols) squares
      (\\) = (List.\\)
  in concat $
       map (\ (row, square) ->
             case row `List.intersect` square of
               [] -> []
               common ->
                 [
                   (common, square \\ common, row \\ common),
                   (common, row \\ common, square \\ common)
                 ])
         product

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct as bs =
  [(a, b) | a <- as, b <- bs]

-- Returns a new Puzzle with each Cell initialized according to
-- Setup, which is a string of 81 digits or dashes.
--
fromString :: String -> Puzzle
fromString setup =
  let digits = toDigits setup
      zipped = zip [0..80] digits
  in foldr (\ (cellNumber, digit) this ->
            case digit of
              Nothing -> this
              Just digit -> Puzzle.place this (Unknown.new cellNumber) digit)
       empty
       zipped

-- Given a Setup string, returns a list of Maybe Int for
-- each cell.
--
toDigits :: String -> [Maybe Int]
toDigits setup =
  [case char of
    '-' -> Nothing
    _ -> Just $ Char.digitToInt char
   | char <- setup]

place :: Puzzle -> Unknown -> Int -> Puzzle
place this unknown' digit =
  let cellNumber = Unknown.cellNumber unknown'
  in this {
    placed = Placed.new cellNumber digit : placed this,
    unknown = map (\ u -> Unknown.place u unknown' digit)
      $ filter (\ x -> Unknown.cellNumber x /= cellNumber)
      $ unknown this
  }

remove :: Puzzle -> [Int] -> Puzzle
remove this cellNumbers =
  let remaining = filter (\p -> not $ Placed.cellNumber p `elem` cellNumbers)
        $ placed this
  in foldr (\p accum ->
             Puzzle.place accum
               (Unknown.new $ Placed.cellNumber p)
               (Placed.digit p))
       Puzzle.empty
       remaining

-- Some external code has done some mojo and etermined that it's not actually
-- possible to put the digit into any cells in the list.  Fix up unknowns
-- to reflect this.
--
notPossibleForList :: Puzzle -> Int -> [Int] -> Puzzle
notPossibleForList this digit cellNumbers =
  this {
    unknown = map
      (\ u ->
        if Unknown.cellNumber u `elem` cellNumbers
           then Unknown.removeDigitFromPossible digit u
           else u)
      $ unknown this
  }

-- We've got placed and unknown and we have to combine them into s single
-- single lit of characters ordered by originating cellNumber.
-- This can be done by building two lists of (cellNumber, char) then
-- sorting them together and extractin the Chars.
-- Or maybe we could make a Cell type which would be either Placed or Empty,
-- and have them all sort together and have a function that would return
-- their Char.  Maybe.  But for now, this works.
toString this =
  let p = map (\ x -> (Placed.cellNumber x, Char.intToDigit $ Placed.digit x))
            $ placed this
      u = map (\ x -> (Unknown.cellNumber x, '-'))
            $ unknown this
  in map snd $ List.sort $ p ++ u

-- Returns a string that prints out as a grid of digits.
--
toPuzzleString :: Puzzle -> String
toPuzzleString this =
  unlines $
    map (\puzzles ->
          unlines $
            map (\row -> unwords $ slices 3 row) $
              slices 9 puzzles) $
      slices 27 $ Puzzle.toString this

-- You'd think there would be a function to do this but I can't
-- find one easily.  It will be good practice to roll my own.
-- This can be done all kinds of ways, but here I don't use
-- anything fancy.   Well ok splitAt is fancy.  And I'm building it
-- non-reversed with ++ instead of :.
--
slices :: Int -> [a] -> [[a]]
slices n list =
  slices' n list []

slices' :: Int -> [a] -> [[a]] -> [[a]]
slices' n [] accum = accum
slices' n list accum =
  let (slice, rest) = splitAt n list
  in slices' n rest (accum ++ [slice])
