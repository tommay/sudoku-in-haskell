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

new :: [Heuristic]-> Bool -> SolverOptions
new heuristicList trickySets =
  SolverOptions {
    useHeuristics = null heuristicList,
    heuristics = heuristicList,
    usePermanentTrickySets = trickySets
  }

fast :: SolverOptions
fast = new [] False
