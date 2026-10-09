-- Haskell 2010 Report 9 (PreludeList): head, last, tail, init, null, length, (!!)
module Main (main) where

xs :: [Int]
xs = [10, 20, 30, 40, 50]

main :: IO ()
main = do
  print ("head", head xs, head "a", head [[1 :: Int], []])
  print ("head lazy in tail", head (1 : undefined) :: Int, head [1 :: Int ..])
  print ("last", last xs, last "a", last "abc")
  print ("last lazy in earlier elements", last [undefined, undefined, 3 :: Int])
  print ("tail", tail xs, tail "a", tail "ab")
  print ("tail lazy in head", length (tail [undefined, 1, 2 :: Int]), take 2 (tail [1 :: Int ..]))
  print ("init", init xs, init "a", init "ab")
  print ("init lazy", take 2 (init [1 :: Int ..]), length (init [undefined, undefined :: Int]))
  print ("head : tail", head xs : tail xs == xs, init xs ++ [last xs] == xs)
  print ("null", null ([] :: [Int]), null xs, null "", null [[] :: [Int]], null [undefined :: Int])
  print ("null lazy", null (1 : undefined :: [Int]), null [1 :: Int ..])
  print ("length", length xs, length "", length "hello", length [[1, 2], [3 :: Int]])
  print ("length ignores elements", length [undefined, undefined :: Int], length (replicate 1000 'x'))
  print ("length of range", length [1 .. 100000 :: Int])
  print ("!! first and last", xs !! 0, xs !! 4, xs !! (length xs - 1))
  print ("!! middle", xs !! 2, "hello" !! 1, [[1], [2, 3 :: Int]] !! 1 !! 1)
  print ("!! infinite", [0 :: Int ..] !! 1000, cycle "abc" !! 100, iterate (* 2) (1 :: Integer) !! 70)
  print ("!! lazy in other elements", [undefined, 7, undefined :: Int] !! 1, (undefined : 5 : undefined) !! 1 :: Int)
  print ("!! is infixl 9", xs !! 1 + 1, [xs, xs] !! 1 !! 0)
  print ("!! prefix and sections", (!!) xs 3, (xs !!) 0, map (!! 1) ["ab", "cd"])
  print ("!! all", map (xs !!) [0 .. 4] == xs)
  print ("singleton", head [7 :: Int], last [7 :: Int], tail [7 :: Int], init [7 :: Int], length [7 :: Int], [7 :: Int] !! 0)
