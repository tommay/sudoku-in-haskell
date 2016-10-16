module SolverOptions (
  SolverOptions,
  Heuristic (..),
  new,
  SolverOptions.all,
  useHeuristics,
  heuristics,
  usePermanentTrickySets,
  useGuessing,
) where

data SolverOptions = SolverOptions {
  useHeuristics :: Bool,
  heuristics :: [Heuristic],
  usePermanentTrickySets :: Bool,
  useGuessing :: Bool
} deriving (Show)

data Heuristic =
  EasyPeasy | MissingOne | MissingTwo | Needed | Forced | Tricky
  deriving (Show)

new :: [Heuristic]-> Bool -> Bool -> SolverOptions
new heuristicList trickySets guessing =
  SolverOptions {
    useHeuristics = not $ null heuristicList,
    heuristics = heuristicList,
    usePermanentTrickySets = trickySets,
    useGuessing = guessing
  }

all :: SolverOptions
all = new [] False True
