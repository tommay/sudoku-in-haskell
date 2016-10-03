module Stats (
  Stats,
  Stats.new,
  Stats.guess,
  Stats.guesses,
) where

data Stats = Stats {
  guesses :: Int
} deriving (Show)

new :: Stats
new =
  Stats {
    guesses = 0
  }

guess :: Stats -> Stats
guess this =
  this{ guesses = succ $ Stats.guesses this }
