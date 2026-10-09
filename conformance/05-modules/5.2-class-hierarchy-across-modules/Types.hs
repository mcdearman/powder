module Types (A(..), B(..)) where

import Super
import Sub

data A = A

data B = B Int

instance Super A where
  base A = 1
  twiceBase A = 100

instance Sub A

instance Super B where
  base (B n) = n

instance Sub B where
  extra (B n) = "B" ++ show n
