-- Haskell 2010 Report 15 (Data.Bits): bit, setBit, clearBit, complementBit, testBit
module Main (main) where

import Data.Bits
import Data.Int
import Data.Word

bitsOf :: Int -> [Int]
bitsOf x = [i | i <- [0 .. 63], testBit x i]

main :: IO ()
main = do
  print ("bit", [bit i :: Int | i <- [0 .. 10]])
  print ("bit 62", bit 62 :: Int)
  print ("bit 63 is minBound", (bit 63 :: Int) == minBound)
  print ("bit word8", [bit i :: Word8 | i <- [0 .. 7]])
  print ("bit 7 int8", bit 7 :: Int8)
  print ("bit integer", bit 70 :: Integer)
  print ("setBit", setBit (0 :: Int) 3)
  print ("setBit already set", setBit (8 :: Int) 3)
  print ("setBit sign", setBit (0 :: Int8) 7)
  print ("setBit folded", foldl setBit (0 :: Int) [0, 2, 4])
  print ("clearBit", clearBit (15 :: Int) 0)
  print ("clearBit already clear", clearBit (14 :: Int) 0)
  print ("clearBit sign", clearBit (-1 :: Int8) 7)
  print ("clearBit of minus one", clearBit (-1 :: Int) 0)
  print ("complementBit sets", complementBit (0 :: Int) 5)
  print ("complementBit clears", complementBit (32 :: Int) 5)
  print ("complementBit twice", complementBit (complementBit (77 :: Int) 3) 3)
  print ("complementBit word", complementBit (255 :: Word8) 0)
  print ("testBit", [testBit (5 :: Int) i | i <- [0 .. 3]])
  print ("testBit negative", testBit (-1 :: Int) 63, testBit (-2 :: Int) 0)
  print ("testBit sign", testBit (minBound :: Int) 63, testBit (maxBound :: Int) 63)
  print ("testBit word", testBit (128 :: Word8) 7, testBit (127 :: Word8) 7)
  print ("bits of 1000", bitsOf 1000)
  print ("bits of -1", length (bitsOf (-1)))
  print ("bits of minBound", bitsOf minBound)
  print ("setBit is or with bit", setBit (40 :: Int) 2 == 40 .|. bit 2)
  print ("clearBit is and with complement", clearBit (47 :: Int) 2 == 47 .&. complement (bit 2))
  print ("complementBit is xor with bit", complementBit (47 :: Int) 2 == 47 `xor` bit 2)
  print ("testBit after setBit", [testBit (setBit (0 :: Word16) i) i | i <- [0 .. 15]])
  print ("testBit after clearBit", or [testBit (clearBit (maxBound :: Word16) i) i | i <- [0 .. 15]])
