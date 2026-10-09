-- Haskell 2010 Report 10.3: let blocks inside do, guards and multi-clause local functions inside layout blocks
module Main (main) where

collatz :: Int -> [Int]
collatz = go
  where
    go 1 = [1]
    go n
      | even n = n : go (n `div` 2)
      | otherwise = n : go (3 * n + 1)

main :: IO ()
main = do
  let step n
        | n > 100 = n
        | otherwise = step (n * 2)
      start = 3
  print ("local guarded function", step start)
  let fact :: Integer -> Integer
      fact 0 = 1
      fact n = n * fact (n - 1)
  print ("local multi-clause with signature", fact 20)
  let (a, b) = (1 :: Int, 'x')
      [c, d] = "cd"
  print ("local pattern bindings", a, b, c, d)
  let x = 1 :: Int
  let y = x + 1
  let z1 = y + 1
      z = z1 + 1 in print ("let-in spanning lines in do", z)
  let w = do
        p <- Just x
        q <- Just y
        return (p + q)
  print ("do inside let inside do", w)
  let v = case w of
        Just n | n > 2 -> "big"
               | otherwise -> "small"
        Nothing -> "none"
  print ("case with aligned guards inside let", v)
  print ("collatz", collatz 6)
  let op = (+)
        where
          _unused = ()
      r = 2 `op` (3 :: Int)
  print ("where under a let binding", r)
