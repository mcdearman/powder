-- Haskell 2010 Report 8.5.1, 8.4.3: an import of the address of a C function ("&f"), and a "dynamic" import
-- that calls a function by its address
module Main (main) where

import Foreign.C.Types
import Foreign.Ptr

foreign import ccall "stdlib.h &abs" p_abs :: FunPtr (CInt -> CInt)
foreign import ccall "stdlib.h &labs" p_labs :: FunPtr (CLong -> IO CLong)
foreign import ccall "math.h &sqrt" p_sqrt :: FunPtr (CDouble -> CDouble)
foreign import ccall "math.h &pow" p_pow :: FunPtr (Double -> Double -> Double)
foreign import ccall "dynamic" callInt :: FunPtr (CInt -> CInt) -> CInt -> CInt
foreign import ccall "dynamic" callLong :: FunPtr (CLong -> IO CLong) -> CLong -> IO CLong
foreign import ccall "dynamic" callDouble :: FunPtr (CDouble -> CDouble) -> CDouble -> CDouble
foreign import ccall "dynamic" callDouble2 :: FunPtr (Double -> Double -> Double) -> Double -> Double -> Double

main :: IO ()
main = do
  print ("address is not null", p_abs /= nullFunPtr, p_sqrt /= nullFunPtr)
  print ("an address is the same each time", p_abs == p_abs, castFunPtr p_abs == p_labs)
  print ("call through the address", callInt p_abs (-7), callInt p_abs 7)
  l <- callLong p_labs (-123456789012)
  print ("in IO", l)
  print ("doubles", callDouble p_sqrt 2.25, callDouble2 p_pow 2 10)
  print ("through a Ptr and back", callInt (castPtrToFunPtr (castFunPtrToPtr p_abs)) (-3))
  print ("as a value", map (callInt p_abs) [-2, -1, 0, 1, 2])
