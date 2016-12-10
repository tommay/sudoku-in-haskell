import           Placement (Placement (Placement))
import qualified Puzzle
import qualified Solution
import           Solution (Solution)
import qualified Solver
import qualified SolverOptions
import           SolverOptions (Heuristic (..))
import           Step (Step (Step))

import qualified Text.Regex as Regex
import qualified System.Environment
import qualified System.Random as Random
import qualified Control.Monad as Monad

heuristics = [
  EasyPeasy,
  MissingOne,
  MissingTwo,
  Needed,
  Forced,
  Tricky
  ]

--options = SolverOptions.new heuristics False True
options = SolverOptions.all

-- This is the main function, called from the sudoku script.
-- Initializes Puzzle from the given Filename and prints out solutions
-- if any.
--
main = do
  (filename:_) <- System.Environment.getArgs
  setup <- getSetup filename
  rnd <- Random.getStdGen
  let solutions = Solver.randomSolutions options rnd $ Puzzle.fromString setup
  count <- processAndCount printSolution solutions
  putStrLn $ "There are " ++ show count ++ " solutions."

--Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b

processAndCount :: (a -> IO ()) -> [a] -> IO Int
processAndCount func list =
  Monad.foldM
    (\ accum element -> do
      func element
      return $ accum + 1)
    0
    list

printSolution :: Solution -> IO ()
printSolution solution =
  if False
    then do
      let steps = Solution.steps solution
      mapM_ putStrLn $ map showStep steps
      putStrLn $ unlines
        ["Guesses: " ++ "xxx",
         Puzzle.toPuzzleString $ Solution.puzzle solution]
    else return ()

showStep :: Step -> String
showStep step =
  let Step _ maybePlacement description = step
  in description ++
     case maybePlacement of
       Just (Placement cellNumber digit) ->
         unwords $ [":", show $ rowcol cellNumber, show digit]
       Nothing -> ""

rowcol :: Int -> (Int, Int)
rowcol n =
  (n `div` 9, n `mod` 9)

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
