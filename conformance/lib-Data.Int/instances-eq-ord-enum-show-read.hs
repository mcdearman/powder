-- Haskell 2010 Report 18 (Data.Int): Eq, Ord, Enum, Show and Read instances of the signed integer types
module Main (main) where

import Data.Int

main :: IO ()
main = do
  print ("eq", (5 :: Int8) == 5, (5 :: Int16) /= 6, (minBound :: Int32) == maxBound + 1)
  print ("ord", (-1 :: Int8) < 0, (minBound :: Int16) < maxBound, compare (3 :: Int32) 3, compare (-5 :: Int64) (-6))
  print ("max min", max (-128 :: Int8) 127, min (-128 :: Int8) 127)
  print ("ordering respects sign", maximum [3, -7, 100, -128, 127 :: Int8], minimum [3, -7, 100, -128, 127 :: Int8])
  print ("succ pred", succ (126 :: Int8), pred (-127 :: Int8), succ (-1 :: Int16), pred (0 :: Int64))
  print ("fromEnum", fromEnum (-128 :: Int8), fromEnum (maxBound :: Int16), fromEnum (minBound :: Int32), fromEnum (-1 :: Int64))
  print ("toEnum", (toEnum 127 :: Int8), (toEnum (-32768) :: Int16), (toEnum 65 :: Int32), (toEnum (-1) :: Int64))
  print ("enumFrom stops at maxBound", [125 :: Int8 ..])
  print ("enumFrom Int16", [32765 :: Int16 ..])
  print ("enumFromTo", [-2 .. 2 :: Int8])
  print ("enumFromTo empty", [5 .. 1 :: Int32])
  print ("enumFromTo to maxBound", [maxBound - 2 .. maxBound :: Int64])
  print ("enumFromTo from minBound", [minBound .. minBound + 2 :: Int32])
  print ("enumFromThen up stops at maxBound", [100, 110 :: Int8 ..])
  print ("enumFromThen down stops at minBound", [-100, -110 :: Int8 ..])
  print ("enumFromThenTo", [5, 3 .. -3 :: Int16])
  print ("enumFromThenTo up", [minBound, -64 .. maxBound :: Int8])
  print ("enumFromThenTo not hitting limit", [0, 50 .. 127 :: Int8])
  print ("whole range", length [minBound .. maxBound :: Int8], sum (map toInteger [minBound .. maxBound :: Int8]))
  print ("show", show (0 :: Int8), show (127 :: Int8), show (-128 :: Int8))
  print ("show negative", show (-5 :: Int16), show (minBound :: Int32), show (minBound :: Int64))
  print ("showsPrec 6", showsPrec 6 (-5 :: Int8) "")
  print ("showsPrec 7", showsPrec 7 (-5 :: Int8) "", showsPrec 7 (5 :: Int8) "")
  print ("showsPrec 11", showsPrec 11 (-5 :: Int64) "")
  print (Just (-5 :: Int16))
  print [Just (minBound :: Int8), Nothing]
  print [-1, 0, 1 :: Int32]
  print (-1 :: Int8, -2 :: Int64)
  print ("read", (read "42" :: Int8), (read "-128" :: Int8), (read "32767" :: Int16))
  print ("read 32 64", (read "-2147483648" :: Int32), (read "9223372036854775807" :: Int64), (read "-9223372036854775808" :: Int64))
  print ("read with spaces and parens", (read "  7 " :: Int8), (read "(-7)" :: Int16), (read " ( ( 12 ) ) " :: Int32))
  print ("reads rest", (reads "12 rest" :: [(Int16, String)]))
  print ("reads failure", (reads "x12" :: [(Int16, String)]))
  print ("read . show", all (\x -> read (show x) == x) [minBound .. maxBound :: Int8])
  print ("read in structure", (read "[(1,-2),(-3,4)]" :: [(Int8, Int64)]))
