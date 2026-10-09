module A (v, v, T, T(..), T(C1), K(..), K(m), fromB, module B, module A) where

import B

data T = C1 | C2 deriving Show

class K a where
  m :: a -> Int

instance K T where
  m C1 = 1
  m C2 = 2

v :: Int
v = fromB + 1
