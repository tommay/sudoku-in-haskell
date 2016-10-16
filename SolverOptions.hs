module SolverOptions (
  SolverOptions,
  Heuristic (..),
  new,
  fast,
  useHeuristics,
  heuristics,
  usePermanentTrickySets,
) where

data SolverOptions = SolverOptions {
  useHeuristics :: Bool,
  heuristics :: [Heuristic],
  usePermanentTrickySets :: Bool
} deriving (Show)

data Heuristic =
  EasyPeasy | MissingOne | MissingTwo | Needed | Forced | Tricky
  deriving (Show)

new :: [Heuristic] -> SolverOptions
new heuristicList =
  SolverOptions {
    useHeuristics = null heuristicList,
    heuristics = heuristicList,
    usePermanentTrickySets = False
  }

fast :: SolverOptions
fast =
  SolverOptions {
    useHeuristics = False,
    heuristics = [],
    usePermanentTrickySets = False
  }
