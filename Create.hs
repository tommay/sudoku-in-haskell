import Data.List as List
import qualified System.Random as Random
import qualified System.Environment

import qualified Creater
import qualified Layout
import qualified Puzzle
import           Puzzle (Puzzle)
import qualified Solver
import qualified SolverOptions
import qualified Solution

dothis = doList
solver = Solver.solutions SolverOptions.noGuessing

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
  putStr $ Puzzle.toPuzzleString $ Creater.create rnd layout solver

doList layout = do
  rnd <- Random.getStdGen
  mapM_ putStrLn $ map Puzzle.toPuzzleString $
    Creater.createList rnd layout solver
