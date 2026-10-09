-- Haskell 2010 Report 9: subtract, even, odd, gcd, lcm
module Main (main) where

big :: Integer
big = 2 ^ (64 :: Int)

main :: IO ()
main = do
  print ("subtract", subtract 1 (10 :: Int), subtract 10 (1 :: Int), subtract (-3) (4 :: Int), subtract 0.5 (2 :: Double))
  print ("subtract section", map (subtract 2) [1, 2, 3 :: Int], map (\x -> x - 2) [1, 2, 3 :: Int])
  print ("subtract = flip (-)", subtract 3 10 == flip (-) 3 (10 :: Integer))
  print ("even", map even [-4, -3, -1, 0, 1, 2, 7 :: Int])
  print ("odd", map odd [-4, -3, -1, 0, 1, 2, 7 :: Int])
  print ("even/odd Integer", even big, odd big, even (big + 1), odd (big + 1), odd (negate big - 1))
  print ("even/odd extremes", even (minBound :: Int), odd (maxBound :: Int))
  print ("filter", filter even [1 .. 10 :: Int], filter odd [1 .. 10 :: Int])
  print ("gcd", gcd 12 18 :: Int, gcd 18 12 :: Int, gcd 17 5 :: Int, gcd 100 75 :: Int, gcd 7 7 :: Int)
  print ("gcd negative", gcd (-12) 18 :: Int, gcd 12 (-18) :: Int, gcd (-12) (-18) :: Int, gcd (-4) (-6) :: Integer)
  print ("gcd with zero", gcd 0 5 :: Int, gcd 5 0 :: Int, gcd 0 (-5) :: Int, gcd (-5) 0 :: Int)
  print ("gcd with one", gcd 1 99 :: Int, gcd 99 1 :: Int, gcd 1 1 :: Int)
  print ("gcd Integer", gcd big (big * 3), gcd (big + 1) big, gcd (2 ^ (100 :: Int)) (6 ^ (50 :: Int)) :: Integer)
  print ("lcm", lcm 4 6 :: Int, lcm 6 4 :: Int, lcm 3 5 :: Int, lcm 7 7 :: Int, lcm 1 9 :: Int)
  print ("lcm negative", lcm (-4) 6 :: Int, lcm 4 (-6) :: Int, lcm (-4) (-6) :: Int)
  print ("lcm with zero", lcm 0 5 :: Int, lcm 5 0 :: Int, lcm 0 0 :: Int)
  print ("lcm Integer", lcm big 3, lcm (big + 1) big == (big + 1) * big)
  print ("gcd * lcm", all (\(a, b) -> gcd a b * lcm a b == abs (a * b)) [(4, 6), (-9, 12), (17, 5), (100, 75 :: Int)])
  print ("folds", foldr gcd 0 [12, 18, 30 :: Int], foldr lcm 1 [2, 3, 4, 5 :: Int])
