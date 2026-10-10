-- Haskell 2010 Report 27 (Foreign.C.Types): the C types beyond the common integers: CPtrdiff, CSize,
-- CWchar, CSigAtomic, CLLong, CULLong, CIntPtr, CUIntPtr, CIntMax, CUIntMax, CClock, CTime, CFloat,
-- CDouble. Sizes are those of 64-bit systems with a 64-bit long.
module Main (main) where

import Data.Bits
import Foreign.C.Types
import Foreign.Storable

main :: IO ()
main = do
  print ("sizes", sizeOf (0 :: CPtrdiff), sizeOf (0 :: CSize), sizeOf (0 :: CSigAtomic), sizeOf (0 :: CLLong), sizeOf (0 :: CULLong))
  print ("more sizes", sizeOf (0 :: CIntPtr), sizeOf (0 :: CUIntPtr), sizeOf (0 :: CIntMax), sizeOf (0 :: CUIntMax))
  print ("CPtrdiff", (5 :: CPtrdiff) - 9, minBound == (minBound :: CPtrdiff), maxBound :: CPtrdiff)
  print ("CSize wraps as unsigned", (0 :: CSize) - 1 == maxBound, (maxBound :: CSize) + 1)
  print ("CIntMax and CUIntMax", maxBound :: CIntMax, maxBound :: CUIntMax, minBound :: CIntMax)
  print ("CIntPtr and CUIntPtr", (3 :: CIntPtr) * (-4), (maxBound :: CUIntPtr) == fromIntegral (maxBound :: CSize))
  print ("CSigAtomic", (7 :: CSigAtomic) `div` 2, fromIntegral (maxBound :: CSigAtomic) :: Integer)
  print ("CLLong and CULLong", (minBound :: CLLong) `quot` 2, (maxBound :: CULLong) `shiftR` 60)
  print ("arithmetic classes", toInteger (9 :: CPtrdiff), fromEnum (4 :: CSize), toRational (3 :: CIntMax), [1 .. 3 :: CUIntPtr])
  print ("show and read", show (-12 :: CPtrdiff), read "34" :: CSize, show (5 :: CUIntMax))
  print ("bits", (5 :: CSize) .&. 3, (1 :: CIntPtr) `shiftL` 10, complement (0 :: CUIntMax) == maxBound)
  print ("CTime and CClock", (10 :: CTime) + 5, (3 :: CClock) * 4, (100 :: CTime) > 99)
  print ("CFloat and CDouble", (1.5 :: CFloat) * 2, sqrt (16 :: CDouble), truncate (2.75 :: CDouble) :: Int, realToFrac (0.5 :: CFloat) :: Double)
  print ("conversions between them", fromIntegral (300 :: CInt) :: CSize, fromIntegral (-1 :: CInt) :: CPtrdiff, fromIntegral (65 :: CWchar) :: Int)
