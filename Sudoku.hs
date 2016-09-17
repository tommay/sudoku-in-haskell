import qualified System.FilePath as FilePath
import qualified Text.Regex as Regex
import qualified System.Environment

import qualified Puzzle

-- This is the main function, called from the sudoku script.
-- Initializes Puzzle from the given Filename and prints out solutions
-- if any.
--
main = do
  args <- System.Environment.getArgs
  setup <- getSetup $ head args
  let solutions = Puzzle.solutionsFor setup
  putStrLn $ "There are " ++ (show $ length solutions) ++ " solutions."

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
