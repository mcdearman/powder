module NumInstance () where

import Vec

instance Num Vec where
  Vec a b + Vec c d = Vec (a + c) (b + d)
  Vec a b * Vec c d = Vec (a * c) (b * d)
  negate (Vec a b) = Vec (negate a) (negate b)
  abs (Vec a b) = Vec (abs a) (abs b)
  signum (Vec a b) = Vec (signum a) (signum b)
  fromInteger n = Vec (fromInteger n) (fromInteger n)
