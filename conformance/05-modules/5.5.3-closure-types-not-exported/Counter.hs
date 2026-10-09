module Counter (new, tick, count, describe) where

data Counter = MkCounter Int deriving Show

class Describable a where
  descr :: a -> String

instance Describable Counter where
  descr (MkCounter n) = "counter at " ++ show n

instance Describable Int where
  descr n = "int " ++ show n

new :: Counter
new = MkCounter 0

tick :: Counter -> Counter
tick (MkCounter n) = MkCounter (n + 1)

count :: Counter -> Int
count (MkCounter n) = n

describe :: Describable a => a -> String
describe = descr
