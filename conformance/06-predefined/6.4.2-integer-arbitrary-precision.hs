-- Haskell 2010 Report 6.4 / 6.4.2: Integer is arbitrary precision; arithmetic beyond 2^64
factorial :: Integer -> Integer
factorial n = product [1 .. n]

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

two64 :: Integer
two64 = 2 ^ (64 :: Int)

main :: IO ()
main = do
  print ("2^64", two64)
  print ("2^63 and neighbours", two64 `div` 2 - 1, two64 `div` 2, two64 `div` 2 + 1)
  print ("-2^63 and neighbours", negate (two64 `div` 2) - 1, negate (two64 `div` 2), negate (two64 `div` 2) + 1)
  print ("2^64 neighbours", two64 - 1, two64 + 1)
  print ("2^100", 2 ^ (100 :: Int) :: Integer)
  print ("2^200", 2 ^ (200 :: Int) :: Integer)
  print ("10^40", 10 ^ (40 :: Int) :: Integer)
  print ("25!", factorial 25)
  print ("30!", factorial 30)
  print ("fib 150", fibs !! 150)
  print ("addition carries", two64 - 1 + 1 == two64, (two64 - 1) + (two64 - 1))
  print ("subtraction borrows", two64 - (two64 + 1), 10 ^ (30 :: Int) - 1 :: Integer)
  print ("multiplication", two64 * two64, (two64 - 1) * (two64 - 1), negate two64 * two64)
  print ("multiplication signs", (negate two64) * (negate two64) == two64 * two64, 3 * negate two64)
  print ("squares", (10 ^ (20 :: Int) + 1) ^ (2 :: Int) :: Integer)
  print ("negate/abs/signum", negate two64, abs (negate two64), signum two64, signum (negate two64), signum (two64 - two64))
  print ("comparison", two64 > two64 - 1, negate two64 < negate (two64 - 1), compare (two64 * 2) (two64 + two64), max two64 (two64 + 1))
  print ("equality", two64 * two64 == 2 ^ (128 :: Int), two64 /= two64 + 0)
  print ("show negative big", negate (10 ^ (25 :: Int)) :: Integer, show (negate (10 ^ (25 :: Int)) :: Integer))
  print ("read big", read "-340282366920938463463374607431768211456" == negate (two64 * two64))
  print ("mixed small and big", 1 + two64 * 0, (two64 + 5) - two64, (two64 * 7) `div` two64)
  print ("sum of many", sum (map (two64 *) [1 .. 100]))
  print ("back into small range", fromInteger (two64 * two64 `div` two64 `div` 4294967296) :: Int)
