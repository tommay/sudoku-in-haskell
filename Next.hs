module Next (
  Next (Next)
) where

import Placement (Placement)
import Stats (Stats)

data Next = Next Placement String (Stats -> Stats)
