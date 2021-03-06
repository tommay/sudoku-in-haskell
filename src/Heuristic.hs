import qualified Creater
import qualified Layout
import qualified Puzzle
import           Puzzle (Puzzle)
import qualified Solution
import           Solution (Solution)
import qualified Solver
import qualified SolverOptions
import           SolverOptions (SolverOptions, Heuristic (..))
import qualified Util

import qualified System.IO as IO
import qualified Data.List as List
import qualified System.Random as Random
import qualified System.Environment
import Debug.Trace

data Test = Test String SolverOptions

tests :: [Test]
tests = [
  Test "  Guess" $ SolverOptions.new [] False True,
--  Test "  Nothing" $ SolverOptions.new [] False False,
--  Test "* Nothing" $ SolverOptions.new [] True False,
--  Test "  EasyPeasy" $ SolverOptions.new [EasyPeasy] False False,
--  Test "* EasyPeasy" $ SolverOptions.new [EasyPeasy] True False,
--  Test "  Missing one" $ SolverOptions.new [MissingOne] False False,
--  Test "* Missing one" $ SolverOptions.new [MissingOne] True False,
--  Test "  EasyPeasy + Missing one" $ SolverOptions.new [EasyPeasy, MissingOne] False False,
--  Test "* EasyPeasy + Missing one" $ SolverOptions.new [EasyPeasy, MissingOne] True False,
--  Test "  Missing two" $ SolverOptions.new [MissingTwo] False False,
--  Test "* Missing two" $ SolverOptions.new [MissingTwo] True False,
  Test "  Tricky" $ SolverOptions.new [Tricky] False False,
  Test "* Tricky" $ SolverOptions.new [Tricky] True False,
  -- Needed, Tricky, and Forced can all solve puzzles without help.  They each
  -- solve different sets of puzzles.
  Test "  Needed" $ SolverOptions.new [Needed] False False,
  Test "* Needed" $ SolverOptions.new [Needed] True False,
  Test "  A" $ SolverOptions.new [EasyPeasy, Tricky] False False,
  Test "* A" $ SolverOptions.new [EasyPeasy, Tricky] True False,
  -- B solves more puzzles than A.
  Test "  B" $ SolverOptions.new [EasyPeasy, MissingOne, Tricky] False False,
  Test "* B" $ SolverOptions.new [EasyPeasy, MissingOne, Tricky] True False,
  Test "  C" $ SolverOptions.new [Forced] False False,
  Test "* C" $ SolverOptions.new [Forced] True False,
  -- D is all the heuristics short of permanent trickyset elimination,
  -- but it won't solve everything.
  Test "  D" $ SolverOptions.new [Forced, Needed, Tricky] False False,
  Test "* D" $ SolverOptions.new [Forced, Needed, Tricky] True False
  ]

main = do
  args <- System.Environment.getArgs
  case args of
    [style] ->
      case Layout.getLayout style of
        Just layout -> tryHeuristicsWithLayout layout
        Nothing -> showLayouts
    _ -> showLayouts

showLayouts = do
  putStrLn $
    "Valid layouts:\n" ++ (List.intercalate " " Layout.getLayoutStrings)

tryHeuristicsWithLayout :: [[Int]] -> IO ()
tryHeuristicsWithLayout layout = do
  rnd <- Random.getStdGen
  let counts = zip (repeat 0) tests
  processPuzzles counts $ Util.slices 25 $ Creater.createList rnd layout

processPuzzles :: [(Int, Test)] -> [[Puzzle]] -> IO ()
processPuzzles counts (puzzles:rest) = do
  let incCount :: Puzzle -> (Int, Test) -> (Int, Test)
      incCount puzzle old@(count, test@(Test name options)) =
        if Solver.isSolvableWith options puzzle
          then (count + 1, test)
          else old

      incCounts :: Puzzle -> [(Int, Test)] ->  [(Int, Test)]
      incCounts puzzle =
        map (incCount puzzle)

      newCounts =
        foldr incCounts counts puzzles

      toString (count, Test name _) =
        name ++ ": " ++ show count

  mapM_ putStrLn $ map toString newCounts
  IO.hFlush IO.stdout
  processPuzzles newCounts rest
