import Data.List as List
import qualified System.Random as Random
import qualified System.Random.Shuffle as Shuffle
import qualified System.Environment

import qualified Layout
import qualified Puzzle
import Puzzle (Puzzle)
import qualified Solution
import Solution (Solution)
import qualified Solver

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
  putStr $ Puzzle.toPuzzleString $ create rnd layout

doOneNoGuessing layout = do
  rnd <- Random.getStdGen
  putStr $ Puzzle.toPuzzleString $ createNoGuessing rnd layout

doListNoGuessing layout = do
  rnd <- Random.getStdGen
  mapM_ putStrLn $ map
    (\ (guesses, puzzle) ->
      unlines ["Guesses: " ++ (show guesses),
               Puzzle.toPuzzleString puzzle])
    $ filter (\ (g, p) -> g == 0) $ createList rnd layout

randomSolvedPuzzle :: Random.StdGen -> Puzzle
randomSolvedPuzzle rnd =
  let solution = head $ Solver.randomSolutions Puzzle.empty rnd
  in Solution.puzzle solution

createNoGuessing :: Random.StdGen -> [[Int]] -> Puzzle
createNoGuessing rnd layout =
  head $ take 1 $ map snd $ filter (\ (g, p) -> g == 0) $ createList rnd layout

createList :: Random.StdGen -> [[Int]] -> [(Int, Puzzle)]
createList rnd layout =
  let (rnd1, rnd2) = Random.split rnd
      puzzle = create rnd1 layout
      solution = head $ Solver.solutions puzzle
      guessCount = Solution.guessCount solution
  in (fromEnum guessCount, puzzle):(createList rnd2 layout)

create :: Random.StdGen -> [[Int]] -> Puzzle
create rnd layout =
  let puzzle = randomSolvedPuzzle rnd
      layout' = shuffle rnd layout
  in create' puzzle layout'

create' :: Puzzle -> [[Int]] -> Puzzle
create' puzzle cellNumberLists =
  foldr
    (\ list accum ->
      -- We know accum has only one solution.
      -- Remove more stuff an check if that's still true.
      let newPuzzle = Puzzle.remove accum list
      in case hasOnlyOneSolution newPuzzle of
        True ->
           -- newPuzzle has only one solution, go with it.
          newPuzzle
        False ->
          -- Ooops, removed too much, stick with the original.
          accum)
    puzzle
    cellNumberLists

hasOnlyOneSolution :: Puzzle -> Bool
hasOnlyOneSolution puzzle =
  (length $ take 2 $ Solver.solutions puzzle) == 1

shuffle :: Random.StdGen -> [a] -> [a]
shuffle rnd list =
  Shuffle.shuffle' list (length list) rnd
