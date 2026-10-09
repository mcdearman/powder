-- Haskell 2010 Report 23 (Data.Word): Ix, Bits and Real instances of the unsigned integer types
module Main (main) where

import Data.Array
import Data.Bits
import Data.Word

squares :: Array Word8 Int
squares = listArray (250, 255) [fromIntegral i * fromIntegral i | i <- [250 .. 255 :: Word8]]

checksum :: [Word8] -> Word8
checksum = foldl (\acc b -> (acc `rotateL` 1) `xor` b) 0

main :: IO ()
main = do
  print ("range Word8", range (253, 255 :: Word8))
  print ("range Word16", range (0, 3 :: Word16))
  print ("range Word32 empty", range (3, 1 :: Word32))
  print ("range Word64", range (maxBound - 1, maxBound :: Word64))
  print ("index", index (0, 255 :: Word8) 255, index (10, 20 :: Word16) 15, index (100, 200 :: Word32) 100, index (0, 9 :: Word64) 9)
  print ("inRange", inRange (1, 3 :: Word8) 2, inRange (1, 3 :: Word8) 0, inRange (minBound, maxBound :: Word16) 12345)
  print ("rangeSize", rangeSize (minBound, maxBound :: Word8), rangeSize (minBound, maxBound :: Word16), rangeSize (5, 4 :: Word32))
  print ("tuple of words", range ((0, 254), (1, 255)) :: [(Word8, Word8)])
  print ("array indexed by Word8", squares ! 250, squares ! 255)
  print ("array bounds", bounds squares)
  print ("array show", listArray (0, 1) "ab" :: Array Word16 Char)
  print ("and or xor", (240 :: Word8) .&. 60, (240 :: Word8) .|. 60, (240 :: Word8) `xor` 60)
  print ("complement", complement (0 :: Word8), complement (0 :: Word16), complement (0 :: Word32), complement (0 :: Word64))
  print ("shiftL top bit", (1 :: Word8) `shiftL` 7, (1 :: Word16) `shiftL` 15, (1 :: Word32) `shiftL` 31, (1 :: Word64) `shiftL` 63)
  print ("shiftR logical", (192 :: Word8) `shiftR` 3, (maxBound :: Word16) `shiftR` 12, (maxBound :: Word32) `shiftR` 28, (maxBound :: Word64) `shiftR` 60)
  print ("testBit top", testBit (128 :: Word8) 7, testBit (maxBound :: Word16) 15, testBit (maxBound :: Word32) 31, testBit (maxBound :: Word64) 63)
  print ("isSigned", isSigned (0 :: Word8), isSigned (0 :: Word16), isSigned (0 :: Word32), isSigned (0 :: Word64), isSigned (0 :: Word))
  print ("checksum", checksum [1, 2, 3], checksum (map (fromIntegral . fromEnum) "haskell"))
  print ("bytes of a Word32", [fromIntegral ((305419896 :: Word32) `shiftR` s .&. 255) :: Word8 | s <- [24, 16, 8, 0]])
  print ("toRational", toRational (255 :: Word8), toRational (0 :: Word16), toRational (maxBound :: Word32))
  print ("realToFrac", (realToFrac (200 :: Word8) :: Double), (realToFrac (maxBound :: Word16) :: Float))
  print ("average via fromIntegral", (fromIntegral (sum [250, 251, 252 :: Word16]) / 3 :: Double))
