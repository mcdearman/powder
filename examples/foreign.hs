-- Calling C: functions of numbers, found by name when the program runs.
import Foreign.C.Types

foreign import ccall "math.h sin" c_sin :: Double -> Double

foreign import ccall unsafe "math.h pow" c_pow :: CDouble -> CDouble -> CDouble

foreign import ccall "math.h fma" fma :: Double -> Double -> Double -> Double

foreign import ccall "stdlib.h abs" c_abs :: CInt -> CInt

foreign import ccall "stdlib.h labs" labs :: Int -> Int

foreign import ccall "ctype.h toupper" c_toupper :: CInt -> CInt

foreign import ccall "unistd.h getpid" getpid :: IO CInt

shout :: String -> String
shout = map (toEnum . fromIntegral . c_toupper . fromIntegral . fromEnum)

main :: IO ()
main = do
  print (c_sin 0, c_pow 2 10, fma 2 3 4, c_abs (-5), labs (-7))
  putStrLn (shout "called from c")
  pid <- getpid
  again <- getpid
  print (pid > 0, pid == again, maxBound :: CInt, 2147483647 + 1 :: CInt)
