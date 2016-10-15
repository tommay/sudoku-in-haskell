module SolverOptions (
  SolverOptions,
  new,
  useHeuristics,
  useEasyPeasy,
  useMissingOne,
  useMissingTwo,
  useNeeded,
  useForced,
  useTricky,
  usePermanentTrickySets,
) where

data SolverOptions = SolverOptions {
  useHeuristics :: Bool,

  useEasyPeasy :: Bool,
  useMissingOne :: Bool,
  useMissingTwo :: Bool,
  useNeeded :: Bool,
  useForced :: Bool,
  useTricky :: Bool,

  usePermanentTrickySets :: Bool
} deriving (Show, Eq)

new :: SolverOptions
new =
  SolverOptions {
    useHeuristics = False,

    useEasyPeasy = False,
    useMissingOne = False,
    useMissingTwo = False,
    useNeeded = False,
    useForced = False,
    useTricky = False,

    usePermanentTrickySets = False
  }
