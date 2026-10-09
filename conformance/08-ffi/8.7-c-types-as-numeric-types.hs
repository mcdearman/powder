-- Haskell 2010 Report 8.7 (and chapter 28, Foreign.C.Types): CInt, CUInt, CLong, CDouble, CFloat are numeric types
-- with the usual class instances; values flow between them and foreign functions (assumes 32-bit int, IEEE float/double)
import Foreign.C.Types

foreign import ccall "stdlib.h abs" c_abs :: CInt -> CInt

foreign import ccall "math.h sqrt" c_sqrt :: CDouble -> CDouble

foreign import ccall "math.h sqrtf" c_sqrtf :: CFloat -> CFloat

foreign import ccall "math.h floorf" c_floorf :: CFloat -> CFloat

foreign import ccall "math.h ldexp" c_ldexp :: CDouble -> CInt -> CDouble

sumOfSquares :: [CInt] -> CInt
sumOfSquares xs = sum [x * x | x <- xs]

main :: IO ()
main = do
  print ("CInt arithmetic", fromIntegral (3 + 4 * 5 :: CInt) :: Int, fromIntegral (7 `div` 2 :: CInt) :: Int, fromIntegral ((-7) `mod` 3 :: CInt) :: Int)
  print ("CInt bounds", toInteger (minBound :: CInt), toInteger (maxBound :: CInt))
  print ("CInt wraps at 32 bits", toInteger ((maxBound :: CInt) + 1), toInteger (fromIntegral (2 ^ (32 :: Int) + 5 :: Integer) :: CInt))
  print ("CUInt", toInteger (maxBound :: CUInt), toInteger ((0 :: CUInt) - 1), toInteger (fromIntegral (-1 :: Int) :: CUInt))
  print ("CLong", toInteger (fromIntegral (123456789 :: Int) * 1000 :: CLong))
  print ("Eq and Ord", (3 :: CInt) == 3, (3 :: CInt) < 4, compare (2.5 :: CDouble) 1.5, max (1 :: CLong) 2 == 2)
  print ("Enum", map toInteger [1 .. 5 :: CInt], fromEnum (65 :: CInt), toInteger (succ (9 :: CInt)))
  print ("Show", show (42 :: CInt), show (-7 :: CLong), show (2.5 :: CDouble), show (1.5 :: CFloat), show (7 :: CUInt))
  print ("Read", toInteger (read "123" :: CInt), realToFrac (read "0.5" :: CDouble) :: Double)
  print ("CDouble arithmetic", realToFrac (1.5 * 4 + 0.25 :: CDouble) :: Double, realToFrac (7 / 2 :: CDouble) :: Double)
  print ("CDouble rounding", truncate (2.75 :: CDouble) :: Int, round (2.5 :: CDouble) :: Int, floor (-0.5 :: CDouble) :: Int)
  print ("CFloat", realToFrac (c_sqrtf 16) :: Float, realToFrac (c_floorf 2.5) :: Float, realToFrac (0.5 + 0.25 :: CFloat) :: Double)
  print ("conversions", fromIntegral (c_abs (-6)) * (0.5 :: Double), realToFrac (c_sqrt (fromIntegral (c_abs (-49)))) :: Double)
  print ("mixed foreign call", realToFrac (c_ldexp (c_sqrt 9) (c_abs (-3))) :: Double)
  print ("overloaded code at a C type", toInteger (sumOfSquares [1, 2, 3]), toInteger (sumOfSquares (map c_abs [-1, -2])))
  print ("lists of C values", map toInteger (filter even [1 .. 10 :: CInt]), toInteger (product [1 .. 10 :: CInt]))
