module SolverOptions (
  SolverOptions,
  Heuristic (..),
  new,
  SolverOptions.all,
  noGuessing,
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

-- EasyPeasy: An easy pattern to spot visually. where two rows or columns
--   in a 3-stripe contain a digit and there is only one place in the
--   remaining column where it can go.  This is a subset of Needed but is
--   easy to spot.
-- MissingOne: A set is missing only one digit.  This is a subset of
--   both Needed and Forced, but is easier to spot.
-- MissingTwo: A set is missing two digits, and at least one is Forced.  The
--   remaining digit will eventually be found by MissongOne or some other
--   means.
-- Tricky: A square is missing a digit and there is only one row or column
--   where it can go.  There is not enough information to place the digit
--   in the square, but digit can be reliminated as possibility for the
--   rest of the row or column.  For the heuristiv we just check whether
--   the containing squares need the digit and there is now just one place
--   for it in the square.
-- Needed: A set doesn't have a digit and there is only one place it can go.
-- Forced: A cell has only one possibility, all others having been eliminated.
--   This is most tedious to spot.
--
-- Of the heurisrics, only Needed, Tricky, and Forced are capable of
-- solving some puzzles when used on their own, and none is a subset
-- of any other nor the combination of the others.
--
-- Whereas the Tricky heuristic removes possibilities only while checking
-- for needed digits, the usePermanentTrickySets option will eliminate
-- the possibilities for the rest of the solution steps, and will also
-- eliminate using "inverse" TrickySets in which possibilities are removed
-- from the remainder of a square intead of a row/column.  The idea is to
-- get the strongest solver that doesn't require guessing, even though
-- it's impossible to solve these puzzles visually.

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

-- Try Forced first because it's fast.  EasyPeasy, MissingOne, and MissingTwo
-- are redundant with Forced.
--
noGuessing :: SolverOptions
noGuessing = new [Forced, Needed, Tricky] False False
