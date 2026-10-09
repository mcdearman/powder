-- Haskell 2010 Report 23 (Data.Word): Word, Word8, Word16, Word32, Word64 bounds (Bounded) and unsigned
-- modular arithmetic (Num), including 0 - 1
module Main (main) where

import Data.Word

main :: IO ()
main = do
  print ("Word8 bounds", (minBound :: Word8), (maxBound :: Word8))
  print ("Word16 bounds", (minBound :: Word16), (maxBound :: Word16))
  print ("Word32 bounds", (minBound :: Word32), (maxBound :: Word32))
  print ("Word64 bounds", (minBound :: Word64), (maxBound :: Word64))
  print ("Word bounds", (minBound :: Word), (maxBound :: Word))
  print ("zero minus one", (0 :: Word8) - 1, (0 :: Word16) - 1, (0 :: Word32) - 1, (0 :: Word64) - 1, (0 :: Word) - 1)
  print ("max plus one", (maxBound :: Word8) + 1, (maxBound :: Word16) + 1, (maxBound :: Word32) + 1, (maxBound :: Word64) + 1, (maxBound :: Word) + 1)
  print ("Word8 add wraps", (200 :: Word8) + 100)
  print ("Word8 subtract wraps", (3 :: Word8) - 5)
  print ("Word8 multiply wraps", (16 :: Word8) * 16, (17 :: Word8) * 17, (255 :: Word8) * 255)
  print ("Word8 negate", negate (1 :: Word8), negate (0 :: Word8), negate (255 :: Word8), negate (128 :: Word8))
  print ("Word8 abs is identity", abs (200 :: Word8), abs (0 :: Word8))
  print ("Word8 signum", signum (200 :: Word8), signum (0 :: Word8))
  print ("Word8 fromInteger wraps", (fromInteger 256 :: Word8), (fromInteger 1000 :: Word8), (fromInteger (-1) :: Word8), (fromInteger (-256) :: Word8))
  print ("Word8 power", (2 :: Word8) ^ (7 :: Int), (2 :: Word8) ^ (8 :: Int), (3 :: Word8) ^ (6 :: Int))
  print ("Word16 multiply wraps", (256 :: Word16) * 256, (300 :: Word16) * 300)
  print ("Word16 negate", negate (1 :: Word16))
  print ("Word16 fromInteger wraps", (fromInteger 65536 :: Word16), (fromInteger (-2) :: Word16))
  print ("Word32 multiply wraps", (65536 :: Word32) * 65536, (65537 :: Word32) * 65537)
  print ("Word32 negate", negate (1 :: Word32))
  print ("Word32 power", (2 :: Word32) ^ (31 :: Int), (2 :: Word32) ^ (32 :: Int))
  print ("Word64 multiply wraps", (4294967296 :: Word64) * 4294967296, (4294967297 :: Word64) * 4294967297)
  print ("Word64 negate", negate (1 :: Word64))
  print ("Word64 power", (2 :: Word64) ^ (63 :: Int), (2 :: Word64) ^ (64 :: Int))
  print ("Word64 fromInteger wraps", (fromInteger (2 ^ (64 :: Int) + 9) :: Word64), (fromInteger (-1) :: Word64))
  print ("Word subtract wraps", (5 :: Word) - 6 == maxBound)
  print ("Word multiply wraps", (maxBound :: Word) * 2 == maxBound - 1)
  print ("x plus negate x", [x + negate x | x <- [0, 1, 77, 255 :: Word8]])
  print ("sum wraps", sum (replicate 3 (100 :: Word8)))
  print ("product wraps", product [1 .. 6 :: Word8])
