-- Haskell 2010 Report 8.4 (impent): the optional safety annotation `safe` or `unsafe` on foreign import; default is safe
import Foreign.C.Types

foreign import ccall safe "math.h floor" floor_safe :: CDouble -> CDouble

foreign import ccall unsafe "math.h floor" floor_unsafe :: CDouble -> CDouble

foreign import ccall "math.h floor" floor_default :: CDouble -> CDouble

foreign import ccall safe "math.h pow" pow_safe :: CDouble -> CDouble -> CDouble

foreign import ccall unsafe "math.h pow" pow_unsafe :: CDouble -> CDouble -> CDouble

foreign import ccall safe "stdlib.h abs" abs_safe_io :: CInt -> IO CInt

foreign import ccall unsafe "stdlib.h abs" abs_unsafe_io :: CInt -> IO CInt

foreign import ccall unsafe "math.h fma" fma_unsafe :: CDouble -> CDouble -> CDouble -> CDouble

foreign import ccall safe "math.h fma" fma_safe :: CDouble -> CDouble -> CDouble -> CDouble

-- `safe` and `unsafe` are only special in this position: they remain usable as ordinary identifiers
safe :: Int
safe = 1

unsafe :: Int
unsafe = 2

toD :: CDouble -> Double
toD = realToFrac

main :: IO ()
main = do
  print ("floor", toD (floor_safe 2.5), toD (floor_unsafe 2.5), toD (floor_default 2.5))
  print ("floor negative", toD (floor_safe (-2.5)), toD (floor_unsafe (-2.5)), toD (floor_default (-2.5)))
  print ("pow", toD (pow_safe 2 8), toD (pow_unsafe 2 8))
  print ("fma", toD (fma_safe 3 4 5), toD (fma_unsafe 3 4 5))
  a <- abs_safe_io (-7)
  b <- abs_unsafe_io (-7)
  print ("abs in IO", fromIntegral a :: Int, fromIntegral b :: Int)
  print ("all variants agree", and [floor_safe x == floor_unsafe x && floor_unsafe x == floor_default x | x <- [-3.5, -1, 0.25, 0.5, 2, 9.75]])
  print ("many unsafe calls", toD (sum (map floor_unsafe [0.5, 1.5 .. 999.5])))
  print ("many safe calls", toD (sum (map floor_safe [0.5, 1.5 .. 999.5])))
  print ("safe and unsafe as identifiers", safe, unsafe, safe + unsafe)
