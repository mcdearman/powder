-- Haskell 2010 Report 6.4.2: div/mod/quot/rem/divMod/quotRem at Integer, including operands beyond 64 bits
row :: Integer -> Integer -> (Integer, Integer, Integer, Integer, Integer, Integer)
row x y = (x, y, x `div` y, x `mod` y, x `quot` y, x `rem` y)

big :: Integer
big = 2 ^ (70 :: Int) + 12345

small :: Integer
small = 10 ^ (9 :: Int) + 7

xs :: [Integer]
xs = [big, negate big, big * big + 1, negate (big * big) - 1, 17, -17, 0]

ys :: [Integer]
ys = [small, negate small, 3, -3, big - 1, 1 - big, 2 ^ (64 :: Int), negate (2 ^ (64 :: Int))]

digits :: Integer -> [Integer]
digits 0 = []
digits n = digits (n `div` 10) ++ [n `mod` 10]

main :: IO ()
main = do
  putStrLn "(x, y, div, mod, quot, rem)"
  mapM_ (print . uncurry row) [(7, 2), (-7, 2), (7, -2), (-7, -2)]
  mapM_ (print . uncurry row) [(big, 3), (negate big, 3), (big, -3), (negate big, -3)]
  mapM_ (print . uncurry row) [(big, small), (negate big, small), (big, negate small), (negate big, negate small)]
  mapM_ (print . uncurry row) [(3, big), (-3, big), (3, negate big), (-3, negate big)]
  mapM_ (print . uncurry row) [(big * big, big), (big * big + 1, big), (negate (big * big) - 1, big)]
  print ("divMod", divMod (negate big) small, divMod big (negate small))
  print ("quotRem", quotRem (negate big) small, quotRem big (negate small))
  print ("quot/rem law", and [(x `quot` y) * y + (x `rem` y) == x | x <- xs, y <- ys])
  print ("div/mod law", and [(x `div` y) * y + (x `mod` y) == x | x <- xs, y <- ys])
  print ("divMod agrees", and [divMod x y == (x `div` y, x `mod` y) | x <- xs, y <- ys])
  print ("quotRem agrees", and [quotRem x y == (x `quot` y, x `rem` y) | x <- xs, y <- ys])
  print ("mod has sign of divisor", and [x `mod` y == 0 || signum (x `mod` y) == signum y | x <- xs, y <- ys])
  print ("rem has sign of dividend", and [x `rem` y == 0 || signum (x `rem` y) == signum x | x <- xs, y <- ys])
  print ("across the Int boundary", (2 ^ (63 :: Int)) `div` (-1) :: Integer, (negate (2 ^ (63 :: Int))) `quot` (-1) :: Integer)
  print ("powers of two", (2 ^ (100 :: Int)) `div` (2 ^ (98 :: Int)) :: Integer, (2 ^ (100 :: Int) - 1) `mod` (2 ^ (64 :: Int)) :: Integer)
  print ("decimal digits by div and mod", digits (10 ^ (25 :: Int) + 987654321))
