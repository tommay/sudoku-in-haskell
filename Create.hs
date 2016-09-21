import qualified System.Random as Random
import qualified System.Random.Shuffle as Shuffle

import qualified Layout
import qualified Puzzle
import Puzzle (Puzzle)

layout = Layout.layout Layout.spinny

main = mainOneNoGuessing

mainOne = do
  rnd <- Random.getStdGen
  putStr $ Puzzle.toPuzzleString $ create rnd layout

mainOneNoGuessing = do
  rnd <- Random.getStdGen
  putStr $ Puzzle.toPuzzleString $ createNoGuessing rnd layout

mainListNoGuessing = do
  rnd <- Random.getStdGen
  mapM_ putStrLn $ map
    (\ (guesses, puzzle) ->
      unlines ["Guesses: " ++ (show guesses),
               Puzzle.toPuzzleString puzzle])
    $ filter (\ (g, p) -> g == 0) $ createList rnd layout

randomSolvedPuzzle :: Random.StdGen -> Puzzle
randomSolvedPuzzle rnd =
  let (_, puzzle) = head $ Puzzle.randomSolutions Puzzle.empty rnd
  in puzzle

createNoGuessing :: Random.StdGen -> [[Int]] -> Puzzle
createNoGuessing rnd layout =
  head $ take 1 $ map snd $ filter (\ (g, p) -> g == 0) $ createList rnd layout

createList :: Random.StdGen -> [[Int]] -> [(Int, Puzzle)]
createList rnd layout =
  let (rnd1, rnd2) = Random.split rnd
      puzzle = create rnd1 layout
      (guesses, _) = head $ Puzzle.solutions puzzle
  in (guesses, puzzle):(createList rnd2 layout)

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
  (length $ take 2 $ Puzzle.solutions puzzle) == 1

shuffle :: Random.StdGen -> [a] -> [a]
shuffle rnd list =
  Shuffle.shuffle' list (length list) rnd
