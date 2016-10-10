module Next (
  Next (Next),
  new,
) where

import Digit (Digit)
import Placement (Placement (Placement))
import Stats (Stats)
import Unknown (Unknown)
import qualified Unknown

data Next = Next Placement String (Stats -> Stats)

new :: String -> (Stats -> Stats) -> Digit -> Unknown -> Next
new description statsFunc digit unknown =
  Next (Placement (Unknown.cellNumber unknown) digit)
       description
       statsFunc
