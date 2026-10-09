-- Haskell 2010 Report 15 (Data.Bits): Bits at Integer (unbounded two's complement, large shifts,
-- rotate behaves as shift)
module Main (main) where

import Data.Bits

big :: Integer
big = 2 ^ (100 :: Int) + 5

main :: IO ()
main = do
  print ("and", (12 :: Integer) .&. 10)
  print ("or", (12 :: Integer) .|. 10)
  print ("xor", (12 :: Integer) `xor` 10)
  print ("complement", complement (5 :: Integer))
  print ("complement negative", complement (-6 :: Integer))
  print ("complement zero", complement (0 :: Integer))
  print ("and with minus one", (-1 :: Integer) .&. 255)
  print ("and negatives", (-4 :: Integer) .&. (-6))
  print ("or negatives", (-4 :: Integer) .|. (-6))
  print ("xor negatives", (-4 :: Integer) `xor` (-6))
  print ("xor mixed sign", (-4 :: Integer) `xor` 6)
  print ("shiftL 100", (1 :: Integer) `shiftL` 100)
  print ("shiftL 200", (3 :: Integer) `shiftL` 200)
  print ("shiftL negative value", (-1 :: Integer) `shiftL` 70)
  print ("shiftR big", big `shiftR` 98)
  print ("shiftR past all bits", big `shiftR` 500)
  print ("shiftR negative is arithmetic", (-8 :: Integer) `shiftR` 1, (-7 :: Integer) `shiftR` 1)
  print ("shiftR negative past all bits", (-5 :: Integer) `shiftR` 1000)
  print ("shift negative amount", shift big (-100))
  print ("shift positive amount", shift (5 :: Integer) 64)
  print ("round trip", (big `shiftL` 300) `shiftR` 300 == big)
  print ("rotate is shift left", rotate (5 :: Integer) 66 == shift 5 66)
  print ("rotate is shift right", rotate big (-3) == shift big (-3))
  print ("rotateL", rotateL (1 :: Integer) 65)
  print ("rotateR", rotateR big 100)
  print ("bit 100", bit 100 == (2 :: Integer) ^ (100 :: Int))
  print ("setBit high", setBit (0 :: Integer) 80)
  print ("clearBit high", clearBit big 100)
  print ("complementBit high", complementBit (5 :: Integer) 100 == big)
  print ("testBit big", testBit big 100, testBit big 99, testBit big 0, testBit big 1)
  print ("testBit negative far", testBit (-1 :: Integer) 200, testBit (-2 :: Integer) 0)
  print ("mask low 64 bits", big .&. (2 ^ (64 :: Int) - 1))
  print ("mask of negative", (-big) .&. (2 ^ (8 :: Int) - 1))
  print ("isSigned", isSigned (0 :: Integer))
