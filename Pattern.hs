import qualified Creater
import           Digit (Digit)
import qualified Placed
import qualified Puzzle
import           Puzzle (Puzzle)
import qualified Solution
import qualified Solver
import qualified Stats

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Text.Regex as Regex
import qualified System.Random as Random
import qualified System.Environment

main = do
  (filename:_) <- System.Environment.getArgs
  layout <- getLayout filename
  let size = 81 - (length $ head layout)
  rnd <- Random.getStdGen
  let puzzle = head
        $ filter ((== size) . Puzzle.size)
        $ createListNoGuessing rnd layout
  putStrLn $ Puzzle.toPuzzleString puzzle
  putStrLn $ unlines $ map (flip replicate $ '*') $ List.sort
    $ Map.elems $ count puzzle

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
      cells =  map fst $ filter (\ (_, char) -> char == '-') zipped
  in [cells]

createListNoGuessing :: Random.StdGen -> [[Int]] -> [Puzzle]
createListNoGuessing rnd layout =
  filter
    ((== 0) . Stats.guesses . Solution.stats . head . Solver.solutions)
    $ Creater.createList rnd layout

count :: Puzzle -> Map Digit Int
count puzzle =
  foldr (\ digit ->
          Map.insertWith (+) digit 1)
        Map.empty
        $ map Placed.digit $ Puzzle.placed puzzle
