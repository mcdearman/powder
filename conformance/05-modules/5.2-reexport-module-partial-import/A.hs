module A (module B, module C, module DD, useAll) where

import B (x)
import qualified B (y)
import qualified C
import D as DD

useAll :: [String]
useAll = [x, B.y, C.c, d, DD.d]
