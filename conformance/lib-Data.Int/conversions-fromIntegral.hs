-- Haskell 2010 Report 18 (Data.Int): narrowing and widening conversions between the signed integer types
-- with fromIntegral, toInteger and fromInteger
module Main (main) where

import Data.Int

main :: IO ()
main = do
  print ("Int to Int8 in range", (fromIntegral (100 :: Int) :: Int8), (fromIntegral (-100 :: Int) :: Int8))
  print ("Int to Int8 truncates", (fromIntegral (300 :: Int) :: Int8), (fromIntegral (128 :: Int) :: Int8), (fromIntegral (-129 :: Int) :: Int8))
  print ("Int to Int8 multiples of 256", (fromIntegral (256 :: Int) :: Int8), (fromIntegral (-512 :: Int) :: Int8))
  print ("Int to Int16 truncates", (fromIntegral (70000 :: Int) :: Int16), (fromIntegral (32768 :: Int) :: Int16), (fromIntegral (-32769 :: Int) :: Int16))
  print ("Int to Int32 truncates", (fromIntegral (4294967301 :: Int) :: Int32), (fromIntegral (2147483648 :: Int) :: Int32))
  print ("Int to Int64", (fromIntegral (maxBound :: Int) :: Int64) == maxBound, (fromIntegral (minBound :: Int) :: Int64) == minBound)
  print ("Int64 to Int32 keeps low bits", (fromIntegral (maxBound :: Int64) :: Int32), (fromIntegral (minBound :: Int64) :: Int32))
  print ("Int64 to Int16", (fromIntegral (81985529216486895 :: Int64) :: Int16))
  print ("Int64 to Int8", (fromIntegral (81985529216486895 :: Int64) :: Int8))
  print ("Int32 to Int16", (fromIntegral (maxBound :: Int32) :: Int16), (fromIntegral (minBound :: Int32) :: Int16))
  print ("Int16 to Int8", (fromIntegral (maxBound :: Int16) :: Int8), (fromIntegral (minBound :: Int16) :: Int8), (fromIntegral (384 :: Int16) :: Int8))
  print ("Int8 widening keeps sign", (fromIntegral (-1 :: Int8) :: Int16), (fromIntegral (-1 :: Int8) :: Int32), (fromIntegral (-1 :: Int8) :: Int64), (fromIntegral (-1 :: Int8) :: Int))
  print ("Int8 widening bounds", (fromIntegral (minBound :: Int8) :: Int), (fromIntegral (maxBound :: Int8) :: Int))
  print ("Int16 widening", (fromIntegral (minBound :: Int16) :: Int32), (fromIntegral (maxBound :: Int16) :: Int64))
  print ("Int32 widening", (fromIntegral (minBound :: Int32) :: Int64), (fromIntegral (minBound :: Int32) :: Int))
  print ("to Integer", toInteger (minBound :: Int8), toInteger (maxBound :: Int16), toInteger (minBound :: Int32))
  print ("to Integer 64", toInteger (minBound :: Int64), toInteger (maxBound :: Int64))
  print ("Integer arithmetic past the bounds", toInteger (maxBound :: Int64) + 1, toInteger (minBound :: Int64) - 1)
  print ("from Integer", (fromInteger (-128) :: Int8), (fromInteger 32767 :: Int16), (fromInteger (-2147483648) :: Int32))
  print ("round trip through Integer", (fromInteger (toInteger (minBound :: Int64)) :: Int64) == minBound)
  print ("narrow then widen", (fromIntegral (fromIntegral (1000 :: Int) :: Int8) :: Int))
  print ("widen then narrow", (fromIntegral (fromIntegral (-7 :: Int8) :: Int64) :: Int8))
  print ("to Double", (fromIntegral (-128 :: Int8) :: Double), (fromIntegral (maxBound :: Int16) :: Double), (fromIntegral (minBound :: Int32) :: Double))
  print ("from Double by truncate", (truncate (-3.7 :: Double) :: Int8), (round (2.5 :: Double) :: Int16), (floor (-0.5 :: Double) :: Int32), (ceiling (0.5 :: Double) :: Int64))
  print ("toRational", toRational (-3 :: Int8), toRational (maxBound :: Int32))
  print ("via Char", fromEnum 'A', (fromIntegral (fromEnum 'A') :: Int8))
