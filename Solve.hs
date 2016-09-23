import qualified System.FilePath as FilePath
import qualified Text.Regex as Regex
import qualified System.Environment

import qualified Puzzle
import Puzzle (Puzzle)
import qualified Solution
import Solution (Solution (Solution))
import qualified Solver

data CountedElement a = Count Int | Element a

-- This is the main function, called from the sudoku script.
-- Initializes Puzzle from the given Filename and prints out solutions
-- if any.
--
main = do
  args <- System.Environment.getArgs
  setup <- getSetup $ head args
  let solutions = countedList $ Solver.solutions $ Puzzle.fromString setup
  mapM_ putStrLn $ map countedElementToString solutions

-- Returns the contents of Filename as an IO String with "#" comments
-- and whitespace deleted.  The result should be a string of 81 digits
-- or dashes, where the digits are given by the puzzle and the dash
-- cells are to be solved for.
--
getSetup :: FilePath -> IO String
getSetup filename = do
  raw <- readFile filename
  let
    noComments = Regex.subRegex (Regex.mkRegex "#.*") raw ""
    setup = Regex.subRegex (Regex.mkRegex "\\s+") noComments ""
  return setup

-- Turns a list of as into a list of CountedElements, each of which holds
-- an a except the last one which os added to hold the count of
-- elements in the list.  This allows the list to be treated as a
-- stream and printed on the fly, and when we match on the last element
-- it tells us the number of elements that preceeded it in the list.

countedList ::  [a] -> [CountedElement a]
countedList list = countedList' 0 list

countedList' :: Int -> [a] -> [CountedElement a]
countedList' n [] =
  [Count n]
countedList' n (head : tail) =
  (Element head) : countedList' (n + 1) tail

countedElementToString :: CountedElement Solution -> String
countedElementToString (Count n) =
  "There are " ++ (show n) ++ " solutions."  
countedElementToString (Element (Solution guesses puzzle)) =
  unlines ["Guesses: " ++ show guesses,
           Puzzle.toPuzzleString puzzle]
