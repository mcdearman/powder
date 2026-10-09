-- Haskell 2010 Report 23 (Data.Word): Eq, Ord, Enum, Show and Read instances of the unsigned integer types
module Main (main) where

import Data.Word

main :: IO ()
main = do
  print ("eq", (5 :: Word8) == 5, (5 :: Word16) /= 6, (0 :: Word32) == maxBound + 1)
  print ("ord is unsigned", (200 :: Word8) > 100, (maxBound :: Word64) > 0, compare (128 :: Word8) 127, compare (0 :: Word16) 65535)
  print ("max min", max (0 :: Word8) 255, min (0 :: Word8) 255)
  print ("maximum minimum", maximum [3, 250, 128, 127 :: Word8], minimum [3, 250, 128, 127 :: Word8])
  print ("wrapped value is the largest", (0 :: Word16) - 1 > 1000)
  print ("succ pred", succ (254 :: Word8), pred (1 :: Word8), succ (65534 :: Word16), pred (maxBound :: Word64))
  print ("fromEnum", fromEnum (255 :: Word8), fromEnum (maxBound :: Word16), fromEnum (maxBound :: Word32))
  print ("toEnum", (toEnum 255 :: Word8), (toEnum 65535 :: Word16), (toEnum 65 :: Word32), (toEnum 0 :: Word64))
  print ("enumFrom stops at maxBound", [250 :: Word8 ..])
  print ("enumFrom Word16", [65533 :: Word16 ..])
  print ("enumFromTo", [0 .. 5 :: Word8])
  print ("enumFromTo empty", [5 .. 1 :: Word32])
  print ("enumFromTo to maxBound", [maxBound - 2 .. maxBound :: Word64])
  print ("enumFromThen up stops at maxBound", [0, 100 :: Word8 ..])
  print ("enumFromThen down stops at zero", [10, 7 :: Word8 ..])
  print ("enumFromThenTo down", [10, 7 .. 0 :: Word8])
  print ("enumFromThenTo up", [0, 64 .. maxBound :: Word8])
  print ("enumFromThenTo Word16", [0, 20000 .. maxBound :: Word16])
  print ("whole range", length [minBound .. maxBound :: Word8], sum (map toInteger [minBound .. maxBound :: Word8]))
  print ("show", show (0 :: Word8), show (255 :: Word8), show (maxBound :: Word16), show (maxBound :: Word32))
  print ("show 64", show (maxBound :: Word64), show (maxBound :: Word))
  print ("showsPrec 11 has no parentheses", showsPrec 11 (200 :: Word8) "")
  print (Just (255 :: Word8))
  print [0, 128, 255 :: Word8]
  print (1 :: Word16, 2 :: Word)
  print ("read", (read "0" :: Word8), (read "255" :: Word8), (read "65535" :: Word16))
  print ("read 32 64", (read "4294967295" :: Word32), (read "18446744073709551615" :: Word64), (read "42" :: Word))
  print ("read with spaces and parens", (read "  7 " :: Word8), (read "(7)" :: Word16), (read " ( ( 12 ) ) " :: Word32))
  print ("reads rest", (reads "12 rest" :: [(Word16, String)]))
  print ("reads failure", (reads "x12" :: [(Word16, String)]))
  print ("read . show", all (\x -> read (show x) == x) [minBound .. maxBound :: Word8])
  print ("read in structure", (read "[(1,2),(3,4)]" :: [(Word8, Word64)]))
