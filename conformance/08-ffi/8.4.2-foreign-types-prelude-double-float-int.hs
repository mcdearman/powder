-- Haskell 2010 Report 8.4.2: basic foreign types include the Prelude types Double, Float and Int and the Data.Int types
-- (assumes the usual 64-bit C ABI: C double = Double, float = Float, long = 64-bit Int, int = Int32)
import Data.Int (Int32, Int64)

foreign import ccall "math.h sqrt" sqrtD :: Double -> Double

foreign import ccall "math.h pow" powD :: Double -> Double -> Double

foreign import ccall "math.h fma" fmaD :: Double -> Double -> Double -> Double

foreign import ccall "math.h fabs" fabsD :: Double -> Double

foreign import ccall "math.h fmod" fmodIO :: Double -> Double -> IO Double

foreign import ccall "math.h sqrtf" sqrtF :: Float -> Float

foreign import ccall "math.h fabsf" fabsF :: Float -> Float

foreign import ccall "math.h powf" powF :: Float -> Float -> Float

foreign import ccall "stdlib.h labs" labsI :: Int -> Int

foreign import ccall "stdlib.h llabs" llabs64 :: Int64 -> Int64

foreign import ccall "stdlib.h abs" abs32 :: Int32 -> Int32

foreign import ccall "ctype.h toupper" toupper32 :: Int32 -> Int32

main :: IO ()
main = do
  print ("sqrt at Double", sqrtD 16, sqrtD 2.25, sqrtD 2 == sqrt 2)
  print ("pow at Double", powD 2 10, powD 2 (-1), powD 9 0.5)
  print ("fma at Double", fmaD 2 3 4, fmaD 0.5 4 (-2))
  print ("fabs at Double", fabsD (-3.5), fabsD 3.5, fabsD (-1 / 0))
  m <- fmodIO 7 3
  print ("fmod in IO at Double", m)
  print ("sqrtf at Float", sqrtF 16, sqrtF 0.25)
  print ("fabsf at Float", fabsF (-1.5), fabsF 2)
  print ("powf at Float", powF 2 10, powF 4 0.5)
  print ("labs at Int", labsI (-123456789012), labsI 5, labsI (minBound + 1) == maxBound)
  print ("llabs at Int64", llabs64 (-9000000000000000000), llabs64 0)
  print ("abs at Int32", abs32 (-2147483647), abs32 12, abs32 (-12))
  print ("toupper at Int32", toEnum (fromIntegral (toupper32 (fromIntegral (fromEnum 'q')))) :: Char)
  print ("no conversion needed", sqrtD 9 + 1, fromIntegral (labsI (-4)) * sqrtD 4, floor (powD 2 20) :: Int)
  print ("higher-order", map sqrtD [1, 4, 9], foldr powD 1 [2, 3], zipWith3 fmaD [1, 2] [3, 4] [5, 6])
  print ("negative zero and infinity pass through", isNegativeZero (fabsD 0 * (-1)), isInfinite (powD 10 400), isNaN (sqrtD (-1)))
