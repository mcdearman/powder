{-# LANGUAGE ExistentialQuantification #-}
-- Nor may what was in scope before the match come to be of that type: here a function's argument would
data Counter = forall s. Counter s (s -> s) (s -> Int)

mix :: [Counter] -> Int
mix cs = go Nothing cs
  where
    go kept [] = 0
    go kept (Counter s step value : rest) = case kept of
      Nothing -> go (Just s) rest
      Just other -> value (step other) + go (Just s) rest

main :: IO ()
main = print (mix [Counter 'a' succ fromEnum, Counter (1 :: Int) (+ 1) id])
