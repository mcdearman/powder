-- Haskell 2010 Report 18 (Data.Int): Integral instances: signs of div/mod (towards negative infinity) and
-- quot/rem (towards zero), divMod, quotRem, gcd, lcm, even, odd at every signed type
module Main (main) where

import Data.Int

signs :: (Integral a) => [(a, a)]
signs = [(7, 2), (-7, 2), (7, -2), (-7, -2), (6, 3), (-6, 3), (0, 5), (0, -5), (1, 7), (-1, 7)]

table :: (Integral a, Show a) => String -> [(a, a)] -> IO ()
table name ps = do
  putStrLn (name ++ " div  " ++ show [x `div` y | (x, y) <- ps])
  putStrLn (name ++ " mod  " ++ show [x `mod` y | (x, y) <- ps])
  putStrLn (name ++ " quot " ++ show [x `quot` y | (x, y) <- ps])
  putStrLn (name ++ " rem  " ++ show [x `rem` y | (x, y) <- ps])
  putStrLn (name ++ " divMod law  " ++ show (and [(x `div` y) * y + x `mod` y == x | (x, y) <- ps]))
  putStrLn (name ++ " quotRem law " ++ show (and [(x `quot` y) * y + x `rem` y == x | (x, y) <- ps]))
  putStrLn (name ++ " divMod is (div, mod)   " ++ show (and [divMod x y == (x `div` y, x `mod` y) | (x, y) <- ps]))
  putStrLn (name ++ " quotRem is (quot, rem) " ++ show (and [quotRem x y == (x `quot` y, x `rem` y) | (x, y) <- ps]))

main :: IO ()
main = do
  table "Int8 " (signs :: [(Int8, Int8)])
  table "Int16" (signs :: [(Int16, Int16)])
  table "Int32" (signs :: [(Int32, Int32)])
  table "Int64" (signs :: [(Int64, Int64)])
  table "Int  " (signs :: [(Int, Int)])
  print ("Int8 divMod", divMod (-7 :: Int8) 2, divMod (7 :: Int8) (-2))
  print ("Int8 quotRem", quotRem (-7 :: Int8) 2, quotRem (7 :: Int8) (-2))
  print ("Int8 minBound", (minBound :: Int8) `div` 2, (minBound :: Int8) `quot` 3, (minBound :: Int8) `mod` 3, (minBound :: Int8) `rem` 3)
  print ("Int8 maxBound", (maxBound :: Int8) `div` (-1), (maxBound :: Int8) `mod` (-2), (maxBound :: Int8) `divMod` minBound)
  print ("Int16 minBound", (minBound :: Int16) `divMod` 7, (minBound :: Int16) `quotRem` 7)
  print ("Int32 minBound", (minBound :: Int32) `divMod` 10, (minBound :: Int32) `quotRem` 10)
  print ("Int64 minBound", (minBound :: Int64) `divMod` 10, (minBound :: Int64) `quotRem` 10)
  print ("Int64 maxBound", (maxBound :: Int64) `divMod` (-10), (maxBound :: Int64) `quotRem` (-10))
  print ("gcd", gcd (-12 :: Int8) 18, gcd (12 :: Int16) (-18), gcd (0 :: Int32) 9, gcd (48 :: Int64) 36)
  print ("lcm", lcm (4 :: Int8) 6, lcm (-4 :: Int16) 6, lcm (0 :: Int32) 9, lcm (21 :: Int64) 6)
  print ("even odd", even (-2 :: Int8), odd (-3 :: Int16), even (minBound :: Int32), odd (maxBound :: Int64))
  print ("mod has the sign of the divisor", and [signum ((x :: Int8) `mod` y) `elem` [0, signum y] | (x, y) <- signs])
  print ("rem has the sign of the dividend", and [signum ((x :: Int8) `rem` y) `elem` [0, signum x] | (x, y) <- signs])
