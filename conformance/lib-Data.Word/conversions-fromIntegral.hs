-- Haskell 2010 Report 23 (Data.Word): conversions between unsigned and signed integer types with
-- fromIntegral (including from negative Ints), toInteger and fromInteger
module Main (main) where

import Data.Int
import Data.Word

main :: IO ()
main = do
  print ("negative Int to Word8", (fromIntegral (-1 :: Int) :: Word8), (fromIntegral (-200 :: Int) :: Word8), (fromIntegral (-256 :: Int) :: Word8))
  print ("negative Int to Word16", (fromIntegral (-1 :: Int) :: Word16), (fromIntegral (-32768 :: Int) :: Word16))
  print ("negative Int to Word32", (fromIntegral (-1 :: Int) :: Word32), (fromIntegral (minBound :: Int) :: Word32))
  print ("negative Int to Word64", (fromIntegral (-1 :: Int) :: Word64), (fromIntegral (minBound :: Int) :: Word64))
  print ("negative Int to Word", (fromIntegral (-1 :: Int) :: Word) == maxBound, (fromIntegral (-2 :: Int) :: Word))
  print ("Int to Word8 truncates", (fromIntegral (256 :: Int) :: Word8), (fromIntegral (511 :: Int) :: Word8), (fromIntegral (1000 :: Int) :: Word8))
  print ("Int to Word16 truncates", (fromIntegral (65536 :: Int) :: Word16), (fromIntegral (70000 :: Int) :: Word16))
  print ("Int to Word32 truncates", (fromIntegral (4294967296 :: Int) :: Word32), (fromIntegral (maxBound :: Int) :: Word32))
  print ("Word8 to Int8 reinterprets", (fromIntegral (200 :: Word8) :: Int8), (fromIntegral (128 :: Word8) :: Int8), (fromIntegral (127 :: Word8) :: Int8), (fromIntegral (255 :: Word8) :: Int8))
  print ("Int8 to Word8 reinterprets", (fromIntegral (-1 :: Int8) :: Word8), (fromIntegral (minBound :: Int8) :: Word8), (fromIntegral (-56 :: Int8) :: Word8))
  print ("Word16 to Int16", (fromIntegral (maxBound :: Word16) :: Int16), (fromIntegral (32768 :: Word16) :: Int16))
  print ("Word32 to Int32", (fromIntegral (maxBound :: Word32) :: Int32), (fromIntegral (2147483648 :: Word32) :: Int32))
  print ("Word64 to Int64", (fromIntegral (maxBound :: Word64) :: Int64), (fromIntegral (9223372036854775808 :: Word64) :: Int64))
  print ("Word to Int", (fromIntegral (maxBound :: Word) :: Int))
  print ("Int8 sign extends to wider words", (fromIntegral (-1 :: Int8) :: Word16), (fromIntegral (-1 :: Int8) :: Word32), (fromIntegral (-1 :: Int8) :: Word64))
  print ("Word8 zero extends", (fromIntegral (255 :: Word8) :: Word16), (fromIntegral (255 :: Word8) :: Int), (fromIntegral (255 :: Word8) :: Int16))
  print ("Word32 zero extends", (fromIntegral (maxBound :: Word32) :: Int), (fromIntegral (maxBound :: Word32) :: Word64))
  print ("word narrowing", (fromIntegral (maxBound :: Word64) :: Word32), (fromIntegral (81985529216486895 :: Word64) :: Word16), (fromIntegral (81985529216486895 :: Word64) :: Word8))
  print ("toInteger is non negative", toInteger (maxBound :: Word8), toInteger (maxBound :: Word16), toInteger (maxBound :: Word32))
  print ("toInteger 64", toInteger (maxBound :: Word64), toInteger (maxBound :: Word))
  print ("Integer arithmetic past the bound", toInteger (maxBound :: Word64) + 1)
  print ("fromInteger", (fromInteger 255 :: Word8), (fromInteger 18446744073709551615 :: Word64))
  print ("round trip through Integer", (fromInteger (toInteger (maxBound :: Word64)) :: Word64) == maxBound)
  print ("to Double", (fromIntegral (255 :: Word8) :: Double), (fromIntegral (maxBound :: Word32) :: Double))
  print ("from Double", (truncate (200.9 :: Double) :: Word8), (round (65534.5 :: Double) :: Word16), (ceiling (0.1 :: Double) :: Word32))
  print ("toRational", toRational (255 :: Word8), toRational (maxBound :: Word64))
  print ("char code", (fromIntegral (fromEnum 'z') :: Word8), toEnum (fromIntegral (65 :: Word8)) :: Char)
