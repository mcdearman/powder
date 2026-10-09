module A (L.size, A.own, B.Box(..), B.Label(B.label)) where

import qualified Lib as L
import qualified B

own :: Int
own = L.size "four"
