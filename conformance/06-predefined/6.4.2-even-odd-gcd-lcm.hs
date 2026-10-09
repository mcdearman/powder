-- Haskell 2010 Report 6.4.2: even, odd, gcd, lcm (results are non-negative; zero operands); gcd 0 0 is tested elsewhere
pairsI :: [(Int, Int)]
pairsI = [(x, y) | x <- [-12 .. 12], y <- [-12 .. 12], (x, y) /= (0, 0)]

main :: IO ()
main = do
  print ("even", map even [-4, -3, -1, 0, 1, 2, 7 :: Int])
  print ("odd", map odd [-4, -3, -1, 0, 1, 2, 7 :: Int])
  print ("even/odd Integer", even (2 ^ (70 :: Int) :: Integer), odd (2 ^ (70 :: Int) + 1 :: Integer), odd (negate (3 ^ (50 :: Int)) :: Integer))
  print ("even is not odd", and [even n /= odd n | n <- [-50 .. 50 :: Int]])
  print ("gcd signs", gcd 12 18 :: Int, gcd (-12) 18 :: Int, gcd 12 (-18) :: Int, gcd (-12) (-18) :: Int)
  print ("gcd with zero", gcd 0 5 :: Int, gcd 5 0 :: Int, gcd 0 (-5) :: Int, gcd (-5) 0 :: Int)
  print ("gcd with one", gcd 1 99 :: Int, gcd 99 1 :: Int, gcd (-1) (-99) :: Int)
  print ("gcd misc", gcd 17 5 :: Int, gcd 100 75 :: Int, gcd 7 7 :: Int, gcd (-7) (-7) :: Int, gcd 1071 462 :: Int)
  print ("gcd Integer", gcd (2 ^ (80 :: Int)) (6 ^ (40 :: Int)) :: Integer, gcd (negate (10 ^ (30 :: Int))) (15 ^ (20 :: Int)) :: Integer)
  print ("lcm signs", lcm 4 6 :: Int, lcm (-4) 6 :: Int, lcm 4 (-6) :: Int, lcm (-4) (-6) :: Int)
  print ("lcm with zero", lcm 0 5 :: Int, lcm 5 0 :: Int, lcm 0 0 :: Int, lcm 0 (-3) :: Int)
  print ("lcm misc", lcm 1 9 :: Int, lcm 9 9 :: Int, lcm 21 6 :: Int, lcm 7 13 :: Int)
  print ("lcm Integer", lcm (2 ^ (70 :: Int)) (3 ^ (40 :: Int)) :: Integer, lcm (negate (10 ^ (20 :: Int))) (4 ^ (20 :: Int)) :: Integer)
  print ("gcd non-negative", and [gcd x y > 0 | (x, y) <- pairsI])
  print ("gcd divides both", and [x `mod` gcd x y == 0 && y `mod` gcd x y == 0 | (x, y) <- pairsI])
  print ("gcd commutes", and [gcd x y == gcd y x | (x, y) <- pairsI])
  print ("gcd ignores sign", and [gcd x y == gcd (abs x) (abs y) | (x, y) <- pairsI])
  print ("lcm non-negative", and [lcm x y >= 0 | (x, y) <- pairsI])
  print ("gcd * lcm == abs (x * y)", and [gcd x y * lcm x y == abs (x * y) | (x, y) <- pairsI])
  print ("folds", foldr gcd 0 [12, 18, 30 :: Int], foldr lcm 1 [1 .. 20 :: Integer])
