-- Haskell 2010 Report 8.4: foreign import ccall of C functions with two and three arguments (pow, fmod, atan2, ldexp, fma)
import Foreign.C.Types

foreign import ccall "math.h pow" c_pow :: CDouble -> CDouble -> CDouble

foreign import ccall "math.h fmod" c_fmod :: CDouble -> CDouble -> CDouble

foreign import ccall "math.h atan2" c_atan2 :: CDouble -> CDouble -> CDouble

foreign import ccall "math.h ldexp" c_ldexp :: CDouble -> CInt -> CDouble

foreign import ccall "math.h fma" c_fma :: CDouble -> CDouble -> CDouble -> CDouble

toD :: CDouble -> Double
toD = realToFrac

close :: Double -> Double -> Bool
close a b = abs (a - b) < 1.0e-9

main :: IO ()
main = do
  print ("pow", toD (c_pow 2 10), toD (c_pow 3 4), toD (c_pow 2 (-2)), toD (c_pow 10 0), toD (c_pow 1.5 2))
  print ("pow argument order", toD (c_pow 2 3), toD (c_pow 3 2))
  print ("fmod", toD (c_fmod 7 3), toD (c_fmod (-7) 3), toD (c_fmod 7 (-3)), toD (c_fmod 7.5 2), toD (c_fmod 1 3))
  print ("atan2 exact", toD (c_atan2 0 1))
  print ("atan2 close", close (toD (c_atan2 1 0)) (pi / 2), close (toD (c_atan2 0 (-1))) pi, close (toD (c_atan2 1 1)) (pi / 4), close (toD (c_atan2 (-1) 0)) (negate pi / 2))
  print ("atan2 argument order", toD (c_atan2 1 0) > 1, toD (c_atan2 0 1) < 1)
  print ("ldexp (mixed argument types)", toD (c_ldexp 1.5 4), toD (c_ldexp 1 10), toD (c_ldexp 8 (-3)), toD (c_ldexp 0 5))
  print ("fma", toD (c_fma 2 3 4), toD (c_fma 0.5 0.5 0.25), toD (c_fma (-2) 3 10), toD (c_fma 1 1 1), toD (c_fma 0 5 7))
  print ("fma argument order", toD (c_fma 2 3 100), toD (c_fma 100 2 3), toD (c_fma 3 100 2))
  print ("partial application", map (toD . c_pow 2) [0, 1, 2, 3], map (toD . flip c_fmod 4) [5, 6, 7, 8])
  print ("zipWith", map toD (zipWith c_pow [2, 3, 4] [3, 2, 0.5]), map toD (zipWith3 c_fma [1, 2] [3, 4] [5, 6]))
  print ("infix use", toD (2 `c_pow` 5), toD (9 `c_fmod` 4))
  print ("composition", toD (c_pow (c_fmod 7 4) (c_fma 1 1 1)), toD (c_ldexp (c_pow 2 3) 2))
  print ("agrees with Haskell", toD (c_pow 2 0.5) == sqrt 2 || close (toD (c_pow 2 0.5)) (sqrt 2), toD (c_fmod 17 5) == fromIntegral (17 `rem` 5 :: Int))
