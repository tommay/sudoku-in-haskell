import qualified System.FilePath as FilePath
import qualified Text.Regex as Regex
import qualified System.Environment

import qualified Puzzle
import Puzzle (Puzzle)
import qualified Stats
import Stats (Stats)
import qualified Solution
import Solution (Solution)
import qualified Solver

-- This is the main function, called from the sudoku script.
-- Initializes Puzzle from the given Filename and prints out solutions
-- if any.
--
main = do
  (filename:_) <- System.Environment.getArgs
  setup <- getSetup filename
  let solutions = Solver.solutions $ Puzzle.fromString setup
  count <- processAndCount printSolution solutions
  putStrLn $ "There are " ++ show count ++ " solutions."

processAndCount :: (a -> IO ()) -> [a] -> IO Int
processAndCount func list =
  processAndCount' func 0 list
  where
    processAndCount' _ n [] =
      return n
    processAndCount' func n (head:tail) = do
      func head
      processAndCount' func (n + 1) tail

printSolution :: Solution -> IO ()
printSolution solution =
  if False
    then putStrLn $ unlines
      ["Guesses: " ++ (show $ Stats.guesses $ Solution.stats solution),
       Puzzle.toPuzzleString $ Solution.puzzle solution]
    else return ()

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
