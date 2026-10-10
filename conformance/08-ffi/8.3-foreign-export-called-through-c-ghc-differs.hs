-- Haskell 2010 Report 8.3, 8.5.1: foreign export makes a function C's, under the name given or its own;
-- here it is called back through that name and through its address. GHC's interpreter, which is what the
-- expected output of the other tests is made with, does not run foreign exports, so this test's is written
-- out: it is what the program prints compiled with GHC.
module Main (main) where

import Foreign
import Foreign.C

foreign export ccall "powder_conformance_triple" triple :: CInt -> CInt
foreign export ccall powder_conformance_shout :: CInt -> IO ()
foreign export ccall "powder_conformance_scale" scale :: CDouble -> CInt -> IO CDouble

foreign import ccall "powder_conformance_triple" c_triple :: CInt -> CInt
foreign import ccall "&powder_conformance_triple" p_triple :: FunPtr (CInt -> CInt)
foreign import ccall "dynamic" callInt :: FunPtr (CInt -> CInt) -> CInt -> CInt
foreign import ccall "powder_conformance_shout" c_shout :: CInt -> IO ()
foreign import ccall "powder_conformance_scale" c_scale :: CDouble -> CInt -> IO CDouble

triple :: CInt -> CInt
triple x = 3 * x

powder_conformance_shout :: CInt -> IO ()
powder_conformance_shout n = putStrLn (replicate (fromIntegral n) '!')

scale :: CDouble -> CInt -> IO CDouble
scale x n = return (x * fromIntegral n)

main :: IO ()
main = do
  print ("called by its C name", c_triple 14, map c_triple [1, 2, 3])
  print ("called by its address", callInt p_triple 5, p_triple /= nullFunPtr)
  print ("the function here is still a function here", triple 2)
  c_shout 3
  c_shout 0
  s <- c_scale 1.5 4
  print ("two arguments, in IO", s)
