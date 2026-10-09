-- Haskell 2010 Report 18 (Data.Int): Ix, Bits and Real instances of the signed integer types
module Main (main) where

import Data.Array
import Data.Bits
import Data.Int

temps :: Array Int8 String
temps = listArray (-1, 1) ["cold", "mild", "warm"]

main :: IO ()
main = do
  print ("range Int8", range (-2, 2 :: Int8))
  print ("range Int16", range (32765, maxBound :: Int16))
  print ("range Int32 empty", range (3, 1 :: Int32))
  print ("range Int64", range (minBound, minBound + 2 :: Int64))
  print ("index", index (-128, 127 :: Int8) 0, index (10, 20 :: Int16) 15, index (-5, 5 :: Int32) (-5), index (0, 9 :: Int64) 9)
  print ("inRange", inRange (-1, 1 :: Int8) 0, inRange (-1, 1 :: Int8) 2, inRange (minBound, maxBound :: Int16) 0)
  print ("rangeSize", rangeSize (minBound, maxBound :: Int8), rangeSize (minBound, maxBound :: Int16), rangeSize (5, 4 :: Int32))
  print ("tuple of sized ints", range ((0, -1), (1, 0)) :: [(Int8, Int16)])
  print ("array indexed by Int8", temps ! (-1), temps ! 0, temps ! 1)
  print ("array bounds", bounds temps)
  print ("array show", temps)
  print ("and or xor", (-16 :: Int8) .&. 60, (-16 :: Int8) .|. 60, (-16 :: Int8) `xor` 60)
  print ("complement", complement (0 :: Int8), complement (0 :: Int16), complement (0 :: Int32), complement (0 :: Int64))
  print ("shiftL sign bit", (1 :: Int8) `shiftL` 7, (1 :: Int16) `shiftL` 15, (1 :: Int32) `shiftL` 31)
  print ("shiftR arithmetic", (-64 :: Int8) `shiftR` 3, (-64 :: Int16) `shiftR` 3, (-64 :: Int32) `shiftR` 3, (-64 :: Int64) `shiftR` 3)
  print ("testBit sign", testBit (-1 :: Int8) 7, testBit (-1 :: Int16) 15, testBit (-1 :: Int32) 31, testBit (-1 :: Int64) 63)
  print ("isSigned", isSigned (0 :: Int8), isSigned (0 :: Int16), isSigned (0 :: Int32), isSigned (0 :: Int64), isSigned (0 :: Int))
  print ("toRational", toRational (-128 :: Int8), toRational (0 :: Int16), toRational (minBound :: Int64))
  print ("realToFrac", (realToFrac (-3 :: Int8) :: Double), (realToFrac (maxBound :: Int16) :: Float))
  print ("average via fromIntegral", (fromIntegral (sum [1, 2, 4 :: Int16]) / 2 :: Double))
