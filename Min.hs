import qualified Creater
import qualified Layout
import qualified Puzzle
import           Puzzle (Puzzle)
import qualified Solution
import           Solution (Solution)
import qualified Solver
import qualified Stats
import           Stats (Stats)

import qualified System.IO as IO
import qualified Data.List as List
import qualified System.Random as Random
import qualified System.Environment

main = do
  args <- System.Environment.getArgs
  case args of
    [style] ->
      case Layout.getLayout style of
        Just layout -> doMin layout
        Nothing -> showLayouts
    _ -> showLayouts

showLayouts = do
  putStrLn $
    "Valid layouts:\n" ++ (List.intercalate " " Layout.getLayoutStrings)

doMin layout = do
  rnd <- Random.getStdGen
  processAndMin 82 $ createListNoGuessing rnd layout

createListNoGuessing :: Random.StdGen -> [[Int]] -> [Puzzle]
createListNoGuessing rnd layout =
  filter
    ((== 0) . Stats.guesses . Solution.stats . head . Solver.solutions)
    $ Creater.createList rnd layout

processAndMin :: Int -> [Puzzle] -> IO ()
processAndMin min (puzzle:rest) = do
  let count = length $ Puzzle.placed puzzle
  if count <= min
    then do
      putStrLn $ "count: " ++ show count ++ "\n" ++
        (Puzzle.toPuzzleString puzzle)
      IO.hFlush IO.stdout
      processAndMin count rest
    else
      processAndMin min rest
