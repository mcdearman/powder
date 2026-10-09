-- Haskell 2010 Report 23 (Data.Word): Integral instances of the unsigned types (div/mod coincide with
-- quot/rem), divMod, quotRem, gcd, lcm, even, odd
module Main (main) where

import Data.Word

pairs :: (Integral a) => [(a, a)]
pairs = [(7, 2), (200, 7), (255, 16), (0, 5), (1, 255), (255, 255), (100, 101), (254, 1)]

table :: (Integral a, Show a) => String -> [(a, a)] -> IO ()
table name ps = do
  putStrLn (name ++ " div  " ++ show [x `div` y | (x, y) <- ps])
  putStrLn (name ++ " mod  " ++ show [x `mod` y | (x, y) <- ps])
  putStrLn (name ++ " div is quot " ++ show (and [x `div` y == x `quot` y | (x, y) <- ps]))
  putStrLn (name ++ " mod is rem  " ++ show (and [x `mod` y == x `rem` y | (x, y) <- ps]))
  putStrLn (name ++ " divMod law  " ++ show (and [(x `div` y) * y + x `mod` y == x | (x, y) <- ps]))
  putStrLn (name ++ " divMod is quotRem " ++ show (and [divMod x y == quotRem x y | (x, y) <- ps]))

main :: IO ()
main = do
  table "Word8 " (pairs :: [(Word8, Word8)])
  table "Word16" (pairs :: [(Word16, Word16)])
  table "Word32" (pairs :: [(Word32, Word32)])
  table "Word64" (pairs :: [(Word64, Word64)])
  table "Word  " (pairs :: [(Word, Word)])
  print ("Word8 high values are not negative", (200 :: Word8) `div` 2, (255 :: Word8) `mod` 2, (128 :: Word8) `quot` 127)
  print ("Word8 divMod", divMod (255 :: Word8) 10, quotRem (255 :: Word8) 10)
  print ("Word16 maxBound", (maxBound :: Word16) `divMod` 256, (maxBound :: Word16) `quotRem` 1000)
  print ("Word32 maxBound", (maxBound :: Word32) `divMod` 65536, (maxBound :: Word32) `quotRem` 10)
  print ("Word64 maxBound", (maxBound :: Word64) `divMod` 10, (maxBound :: Word64) `quotRem` 4294967296)
  print ("Word maxBound", (maxBound :: Word) `divMod` 3)
  print ("wrapped then divided", ((0 :: Word8) - 1) `div` 2, ((0 :: Word32) - 1) `div` 2)
  print ("gcd", gcd (12 :: Word8) 18, gcd (255 :: Word8) 34, gcd (0 :: Word16) 9, gcd (4294967295 :: Word32) 65535)
  print ("lcm", lcm (4 :: Word8) 6, lcm (0 :: Word16) 9, lcm (21 :: Word64) 6)
  print ("even odd", even (254 :: Word8), odd (255 :: Word8), even (maxBound :: Word64), odd (maxBound :: Word))
  print ("digits", map (`mod` 10) (takeWhile (> 0) (iterate (`div` 10) (maxBound :: Word16))))
