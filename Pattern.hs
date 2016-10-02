import qualified Data.List as List
import qualified System.FilePath as FilePath
import qualified Text.Regex as Regex
import qualified System.Random as Random
import qualified System.Environment
import qualified Debug.Trace

import qualified Puzzle
import Puzzle (Puzzle)
import qualified Creater
import qualified Solver
import qualified Solution

main = do
  (filename:_) <- System.Environment.getArgs
  layout <- getLayout filename
  let unknownCount = length $ head layout
  rnd <- Random.getStdGen
  putStrLn $ Puzzle.toPuzzleString $ head
    $ filter ((== unknownCount) . length . Puzzle.unknown)
    $ createListNoGuessing rnd layout

getLayout :: FilePath -> IO [[Int]]
getLayout filename = do
  pattern <- getPattern filename
  return $ toLayout pattern

-- Returns the contents of Filename as an IO String with "#" comments
-- and whitespace deleted.  The result should be a string of 81 dashes
-- and non-dashes, where the non-dashes will be cells to place numbers.
--
getPattern :: FilePath -> IO String
getPattern filename = do
  raw <- readFile filename
  let
    noComments = Regex.subRegex (Regex.mkRegex "#.*") raw ""
    setup = Regex.subRegex (Regex.mkRegex "\\s+") noComments ""
  return setup

-- Converts a pattern string to a list of cells to create.
--
toLayout :: String -> [[Int]]
toLayout string =
  let zipped = zip [0..] string
      cells =  map fst $ filter (\ (n, char) -> char == '-') zipped
  in [cells]

createListNoGuessing :: Random.StdGen -> [[Int]] -> [Puzzle]
createListNoGuessing rnd layout =
  filter
    ((== 0) . Solution.guessCount . head . Solver.solutions)
    $ Creater.createList rnd layout
