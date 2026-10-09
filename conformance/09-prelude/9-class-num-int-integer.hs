-- Haskell 2010 Report 9: class Num at Int and Integer ((+) (-) (*) negate abs signum fromInteger)
module Main (main) where

big :: Integer
big = 2 ^ (100 :: Int)

factorial :: Integer -> Integer
factorial n = product [1 .. n]

main :: IO ()
main = do
  print ("add", (3 :: Int) + 4, (-3 :: Int) + 4, (3 :: Integer) + 4, big + big)
  print ("sub", (3 :: Int) - 10, (3 :: Int) - (-10), (3 :: Integer) - 10, big - big - 1)
  print ("mul", (-3 :: Int) * 4, (-3 :: Int) * (-4), (0 :: Int) * 99, big * big)
  print ("negate", negate (5 :: Int), negate (-5 :: Int), negate (0 :: Int), negate big)
  print ("unary minus", -(5 :: Int), - (2 + 3 :: Int), -(-(7 :: Integer)))
  print ("abs", abs (-5 :: Int), abs (5 :: Int), abs (0 :: Int), abs (negate big) == big)
  print ("signum", signum (-5 :: Int), signum (0 :: Int), signum (5 :: Int))
  print ("signum Integer", signum (negate big), signum (0 :: Integer), signum big)
  print ("fromInteger", fromInteger 42 :: Int, fromInteger (-42) :: Int, fromInteger 42 :: Integer)
  print ("abs * signum", all (\x -> abs x * signum x == x) [-3, -1, 0, 1, 3 :: Int])
  print ("abs * signum Integer", all (\x -> abs x * signum x == x) [negate big, -1, 0, 1, big])
  print ("literals are fromInteger", (7 :: Int) == fromInteger 7, (7 :: Integer) == fromInteger 7)
  print ("Int wraparound add", (maxBound :: Int) + 1, (minBound :: Int) - 1)
  print ("Int wraparound mul", (maxBound :: Int) * 2, (2 :: Int) ^ (63 :: Int), (2 :: Int) ^ (64 :: Int))
  print ("Int minBound", negate (minBound :: Int) == minBound, abs (minBound :: Int) == minBound)
  print ("fromInteger wraps", fromInteger (2 ^ (64 :: Int) + 5) :: Int, fromInteger (2 ^ (63 :: Int)) :: Int)
  print ("Integer exact", factorial 25, (10 :: Integer) ^ (30 :: Int) - 1)
  print ("Integer mixed signs", big * (-1), (-big) * (-big) == big * big, big - 2 * big)
  print ("sums", sum [1 .. 10 :: Int], product [1 .. 10 :: Int], sum (map negate [1, 2, 3 :: Integer]))
