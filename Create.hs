import Data.List as List
import qualified System.Random as Random
import qualified System.Environment

import qualified Creater
import qualified Layout
import qualified Puzzle
import           Puzzle (Puzzle)
import qualified Solver
import qualified Solution
import           Solution (Solution)
import qualified Stats

dothis = doOneNoGuessing

main = do
  args <- System.Environment.getArgs
  case args of
    [style] ->
      case Layout.getLayout style of
        Just layout -> dothis layout
        Nothing -> showLayouts
    _ -> showLayouts

showLayouts = do
  putStrLn $
    "Valid layouts:\n" ++ (List.intercalate " " Layout.getLayoutStrings)

doOne layout = do
  rnd <- Random.getStdGen
  putStr $ Puzzle.toPuzzleString $ Creater.create rnd layout

doOneNoGuessing layout = do
  rnd <- Random.getStdGen
  putStr $ Puzzle.toPuzzleString $ head $ createListNoGuessing rnd layout

doListNoGuessing layout = do
  rnd <- Random.getStdGen
  mapM_ putStrLn $ map Puzzle.toPuzzleString $ createListNoGuessing rnd layout

createListNoGuessing rnd layout =
  filter
    (\ puzzle ->
      let solution = head $ Solver.solutions puzzle
      in (Stats.guesses $ Solution.stats solution) == 0)
    $ Creater.createList rnd layout
