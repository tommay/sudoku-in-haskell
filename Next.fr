module Next (
  Next (Next),
  new,
) where

import           Digit (Digit)
import           Placement (Placement (Placement))
import           Stats (Stats)

data Next = Next Placement String (Stats -> Stats)

new :: String -> (Stats -> Stats) -> Digit -> Int -> Next
new description statsFunc digit cellNumber =
  Next (Placement cellNumber digit)
       description
       statsFunc
