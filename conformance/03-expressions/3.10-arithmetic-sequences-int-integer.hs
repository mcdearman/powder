-- Haskell 2010 Report 3.10: arithmetic sequences [a..], [a,b..], [a..c], [a,b..c] at Int and Integer
module Main (main) where

main :: IO ()
main = do
  let lo = 2 :: Int
      hi = 6 :: Int
  print ("from-to", [1 .. 10] :: [Int])
  print ("from-then-to", [1, 3 .. 10] :: [Int])
  print ("descending", [10, 8 .. 1] :: [Int])
  print ("empty", [5 .. 1] :: [Int])
  print ("empty then", [1, 3 .. 0] :: [Int], [5, 4 .. 6] :: [Int])
  print ("single", [3 .. 3] :: [Int], [1, 3 .. 2] :: [Int], [1, 3 .. 1] :: [Int])
  print ("negative", [-3 .. 3] :: [Int])
  print ("negative limit", [10, 7 .. -5] :: [Int])
  print ("negative step", [0, -1 .. -3] :: [Int])
  print ("zero step", take 3 [1, 1 .. 1] :: [Int], [1, 1 .. 0] :: [Int])
  print ("from", take 5 [1 ..] :: [Int])
  print ("from-then", take 5 [1, 3 ..] :: [Int])
  print ("from-then down", take 4 [10, 8 ..] :: [Int])
  print ("no spaces", [1..5] :: [Int], [1,3..9] :: [Int])
  print ("variables", [lo..hi], [lo,lo+2..hi], [hi,hi-1..lo])
  print ("expressions", [1 + 1 .. 2 * 3] :: [Int])
  print ("integer big", [2 ^ 64 .. 2 ^ 64 + 2] :: [Integer])
  print ("integer down", [10 ^ 20, 10 ^ 20 - 1 .. 10 ^ 20 - 3] :: [Integer])
  print ("integer from", take 3 [2 ^ 70 ..] :: [Integer])
  print ("near maxBound", [maxBound - 2 .. maxBound] :: [Int])
  print ("near minBound", [minBound .. minBound + 2] :: [Int])
  print ("from stops at maxBound", [maxBound - 1 ..] :: [Int])
  print ("from-then stops at maxBound", [maxBound - 4, maxBound - 2 ..] :: [Int])
  print ("from-then stops at minBound", [minBound + 3, minBound + 1 ..] :: [Int])
  print ("length/sum", length ([1 .. 1000] :: [Int]), sum ([1 .. 100] :: [Int]), product ([1 .. 10] :: [Int]))
  -- the translation into the Enum class methods
  print ("enumFrom", take 4 [7 ..] == take 4 (enumFrom (7 :: Int)))
  print ("enumFromThen", take 4 [7, 5 ..] == take 4 (enumFromThen 7 (5 :: Int)))
  print ("enumFromTo", [7 .. 11] == enumFromTo 7 (11 :: Int))
  print ("enumFromThenTo", [7, 9 .. 20] == enumFromThenTo 7 9 (20 :: Int))
