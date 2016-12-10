module Next (
  Next (Next),
  new,
) where

import           Digit (Digit)
import           Placement (Placement (Placement))

data Next = Next Placement String

new :: String -> Digit -> Int -> Next
new description digit cellNumber =
  Next (Placement cellNumber digit) description
