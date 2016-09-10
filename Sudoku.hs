import System.FilePath as FilePath
import Text.Regex as Regex
import System.Environment

import Puzzle

-- This is the main function, called from the sudoku script.
-- Initializes Puzzle from the given Filename and prints out solutions
-- if any.
--
main = do
  args <- System.Environment.getArgs
  setup <- getSetup $ head args
  let
    solutions = Puzzle.getSolutions $ Puzzle.new setup
  putStrLn $ "There are " ++ (show $ length solutions) ++ " solutions"

-- Returns the contents of Filename as an IO String with "#" comments
-- and whitespace deleted.  The result should be a string of 81 digits
-- or dashes, where the digits are given by the puzzle and the dash
-- cells are to be solved for.
--
getSetup :: FilePath -> IO String
getSetup filename = do
  raw <- readFile filename
  let
    noComments = subRegex (mkRegex "#.*") raw ""
    setup = subRegex (mkRegex "\\s+") noComments ""
  return setup
