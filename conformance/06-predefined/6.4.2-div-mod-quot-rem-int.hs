-- Haskell 2010 Report 6.4.2: div/mod (truncate toward -infinity), quot/rem (toward zero), divMod, quotRem at Int
row :: Int -> Int -> (Int, Int, Int, Int, Int, Int)
row x y = (x, y, x `div` y, x `mod` y, x `quot` y, x `rem` y)

xs :: [Int]
xs = [-20 .. 20]

ys :: [Int]
ys = [-7, -6, -5, -4, -3, -2, -1, 1, 2, 3, 4, 5, 6, 7]

main :: IO ()
main = do
  putStrLn "(x, y, div, mod, quot, rem)"
  mapM_ (print . uncurry row) [(7, 2), (-7, 2), (7, -2), (-7, -2)]
  mapM_ (print . uncurry row) [(6, 3), (-6, 3), (6, -3), (-6, -3)]
  mapM_ (print . uncurry row) [(1, 3), (-1, 3), (1, -3), (-1, -3)]
  mapM_ (print . uncurry row) [(0, 5), (0, -5), (5, 1), (-5, 1), (5, -1), (-5, -1)]
  mapM_ (print . uncurry row) [(2, 7), (-2, 7), (2, -7), (-2, -7)]
  print ("divMod", divMod 7 2 :: (Int, Int), divMod (-7) 2 :: (Int, Int), divMod 7 (-2) :: (Int, Int), divMod (-7) (-2) :: (Int, Int))
  print ("quotRem", quotRem 7 2 :: (Int, Int), quotRem (-7) 2 :: (Int, Int), quotRem 7 (-2) :: (Int, Int), quotRem (-7) (-2) :: (Int, Int))
  print ("quot/rem law", and [(x `quot` y) * y + (x `rem` y) == x | x <- xs, y <- ys])
  print ("div/mod law", and [(x `div` y) * y + (x `mod` y) == x | x <- xs, y <- ys])
  print ("divMod agrees", and [divMod x y == (x `div` y, x `mod` y) | x <- xs, y <- ys])
  print ("quotRem agrees", and [quotRem x y == (x `quot` y, x `rem` y) | x <- xs, y <- ys])
  print ("mod has sign of divisor", and [x `mod` y == 0 || signum (x `mod` y) == signum y | x <- xs, y <- ys])
  print ("rem has sign of dividend", and [x `rem` y == 0 || signum (x `rem` y) == signum x | x <- xs, y <- ys])
  print ("remainders smaller than divisor", and [abs (x `mod` y) < abs y && abs (x `rem` y) < abs y | x <- xs, y <- ys])
  print ("infixl 7", 7 `div` 2 * 2 :: Int, 2 * 7 `div` 2 :: Int, 20 `div` 3 `mod` 4 :: Int, 1 + 7 `mod` 4 :: Int)
  print ("unary minus binds looser", -7 `div` 2 :: Int, (-7) `div` 2 :: Int, -7 `mod` 2 :: Int, (-7) `mod` 2 :: Int)
  print ("near the bounds", (maxBound :: Int) `div` 2, (minBound :: Int) `div` 2, (minBound :: Int) `quot` 2, (maxBound :: Int) `mod` 10)
  print ("minBound remainders", (minBound :: Int) `mod` 10, (minBound :: Int) `rem` 10, (minBound :: Int) `mod` (-10), (maxBound :: Int) `mod` (-10))
  print ("prefix forms", div 9 4 :: Int, mod 9 4 :: Int, quot (-9) 4 :: Int, rem (-9) 4 :: Int)
