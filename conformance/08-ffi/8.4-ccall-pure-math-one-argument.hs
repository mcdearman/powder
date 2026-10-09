-- Haskell 2010 Report 8.4: foreign import ccall of pure one-argument C math functions at CDouble (exact results only)
import Foreign.C.Types

foreign import ccall "math.h sin" c_sin :: CDouble -> CDouble

foreign import ccall "math.h cos" c_cos :: CDouble -> CDouble

foreign import ccall "math.h sqrt" c_sqrt :: CDouble -> CDouble

foreign import ccall "math.h fabs" c_fabs :: CDouble -> CDouble

foreign import ccall "math.h floor" c_floor :: CDouble -> CDouble

foreign import ccall "math.h ceil" c_ceil :: CDouble -> CDouble

toD :: CDouble -> Double
toD = realToFrac

main :: IO ()
main = do
  print ("sin 0", toD (c_sin 0))
  print ("cos 0", toD (c_cos 0))
  print ("sqrt", toD (c_sqrt 16), toD (c_sqrt 0), toD (c_sqrt 2.25), toD (c_sqrt 1.0e10))
  print ("fabs", toD (c_fabs (-3.5)), toD (c_fabs 3.5), toD (c_fabs 0), toD (c_fabs (-1.0e300)))
  print ("floor", toD (c_floor 2.7), toD (c_floor (-2.7)), toD (c_floor 5), toD (c_floor (-0.5)), toD (c_floor 0.5))
  print ("ceil", toD (c_ceil 2.1), toD (c_ceil (-2.1)), toD (c_ceil 5), toD (c_ceil 0.5), toD (c_ceil (-1.5)))
  print ("agrees with the Prelude", toD (c_sqrt 2) == sqrt 2, toD (c_floor 7.9) == fromIntegral (floor (7.9 :: Double) :: Int))
  print ("argument computed in Haskell", toD (c_sqrt (3 * 3 + 4 * 4)), toD (c_fabs (realToFrac (negate (2.5 :: Double)))))
  print ("nested foreign calls", toD (c_sqrt (c_fabs (-81))), toD (c_floor (c_sqrt 17)), toD (c_ceil (c_sqrt 17)))
  print ("result used in Haskell arithmetic", toD (c_sqrt 9 + c_sqrt 16), toD (c_fabs (-2) * 3), c_sqrt 4 == 2, c_floor 1.5 < c_ceil 1.5)
  print ("mapped", map (toD . c_sqrt) [1, 4, 9, 16, 25], map (toD . c_fabs) [-1, 0, 1])
