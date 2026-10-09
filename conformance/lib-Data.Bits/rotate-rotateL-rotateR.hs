-- Haskell 2010 Report 15 (Data.Bits): rotate, rotateL, rotateR on fixed-width types
module Main (main) where

import Data.Bits
import Data.Int
import Data.Word
import Numeric (showHex)

hex :: (Integral a, Show a) => a -> String
hex x = showHex x ""

w8 :: Word8
w8 = 129

w16 :: Word16
w16 = 43981

w32 :: Word32
w32 = 305419896

w64 :: Word64
w64 = 9223372036854775809

main :: IO ()
main = do
  print ("word8 rotateL 1", rotateL w8 1)
  print ("word8 rotateR 1", rotateR w8 1)
  print ("word8 rotateL 4", rotateL (18 :: Word8) 4)
  print ("word8 rotate 0", rotate w8 0)
  print ("word8 rotate negative", rotate w8 (-1) == rotateR w8 1)
  print ("word8 rotate positive", rotate w8 3 == rotateL w8 3)
  print ("word8 all rotations", [rotateL (1 :: Word8) k | k <- [0 .. 7]])
  print ("word8 rotateR all", [rotateR (1 :: Word8) k | k <- [0 .. 7]])
  print ("word16 rotateL 4", hex (rotateL w16 4))
  print ("word16 rotateR 4", hex (rotateR w16 4))
  print ("word16 rotateL 8", hex (rotateL w16 8))
  print ("word32 rotateL 8", hex (rotateL w32 8))
  print ("word32 rotateR 8", hex (rotateR w32 8))
  print ("word32 rotateL 16", hex (rotateL w32 16))
  print ("word64 rotateL 1", rotateL w64 1)
  print ("word64 rotateR 1", hex (rotateR w64 1))
  print ("int8 rotateL into sign", rotateL (64 :: Int8) 1)
  print ("int8 rotateL out of sign", rotateL (-127 :: Int8) 1)
  print ("int8 rotateR low bit", rotateR (1 :: Int8) 1)
  print ("int8 minus one", rotateL (-1 :: Int8) 3)
  print ("int16 rotateL", rotateL (16384 :: Int16) 1)
  print ("int16 rotateR", rotateR (3 :: Int16) 1)
  print ("int32 rotateR", rotateR (1 :: Int32) 1 == minBound)
  print ("int32 rotateL", rotateL (minBound :: Int32) 1)
  print ("int64 rotateL", rotateL (minBound :: Int64) 1)
  print ("int64 rotateR", rotateR (2 :: Int64) 2 == minBound)
  print ("int rotateL 63", rotateL (1 :: Int) 63 == minBound)
  print ("int rotateL 1 of minBound", rotateL (minBound :: Int) 1)
  print ("int rotateR", rotateR (1 :: Int) 1 == minBound)
  print ("int rotate negative", rotate (1 :: Int) (-1) == minBound)
  print ("rotate left then right", [rotateR (rotateL w32 k) k == w32 | k <- [0 .. 31]])
  print ("rotate is infixl 8", 1 `rotateL` 2 + 1 :: Word8)
