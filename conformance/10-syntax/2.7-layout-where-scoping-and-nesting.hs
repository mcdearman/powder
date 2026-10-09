-- Haskell 2010 Report 10.3: nested implicit blocks closed together by a dedent; where attached to the right level
module Main (main) where

f :: Int -> Int
f x = case x of
  0 -> a
    where a = 100
  n | even n -> b
    | otherwise -> c
    where
      b = n * 2
      c = n * 3

g :: Int -> Int
g x = outer
  where
    outer = inner + 1
      where
        inner = deep * 2
          where
            deep = x
    unrelated = 0

-- a dedent closing three blocks at once
h :: Int -> Int
h x =
  let a =
        let b =
              let c = x
              in c + 1
        in b + 1
  in a + 1

class Shape a where
  area :: a -> Int
  perimeter :: a -> Int
  perimeter s = p
    where p = area s * 0

data Sq = Sq Int

instance Shape Sq where
  area (Sq n) = sq
    where sq = n * n
  perimeter (Sq n) = let four = 4
                     in four * n

main :: IO ()
main = do
  print ("f", f 0, f 2, f 3)
  print ("g", g 5)
  print ("h", h 0)
  print ("class where", area (Sq 3), perimeter (Sq 3))
  let k n = r
        where r = n + 1
  print ("where inside let in do", k (1 :: Int))
  let j n =
        case n of
          0 -> "zero"
          _ -> "other"
      l = j 0
  print ("case inside let", l, j (1 :: Int))
  print ("do in do", result)
  where
    result = sum $ do
      a <- [1, 2]
      b <- do
        c <- [10, 20]
        return (c + 1)
      return (a * b :: Int)
