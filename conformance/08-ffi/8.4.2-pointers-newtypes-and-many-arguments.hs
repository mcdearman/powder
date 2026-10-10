-- Haskell 2010 Report 8.4.2: foreign types: pointers, the sized integers, Bool-free numeric types, newtypes
-- of foreign types, and functions of more than three arguments
module Main (main) where

import Data.Int
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

newtype Fd = Fd CInt

newtype Size = Size CSize deriving (Eq, Show)

type Buffer = Ptr CChar

foreign import ccall "string.h strlen" c_strlen :: CString -> IO Size
foreign import ccall "string.h memset" c_memset :: Ptr a -> CInt -> CSize -> IO (Ptr a)
foreign import ccall "string.h memcmp" c_memcmp :: Ptr a -> Ptr b -> CSize -> IO CInt
foreign import ccall "string.h strncpy" c_strncpy :: Buffer -> CString -> CSize -> IO Buffer
foreign import ccall "stdio.h snprintf" c_snprintf5 :: Buffer -> CSize -> CString -> CInt -> CInt -> IO CInt
foreign import ccall "stdlib.h llabs" c_llabs :: Int64 -> Int64
foreign import ccall "stdlib.h abs" c_abs32 :: Int32 -> Int32
foreign import ccall "ctype.h toupper" c_toupper :: CInt -> CInt
foreign import ccall "unistd.h close" c_close :: Fd -> IO CInt
foreign import ccall "math.h fma" c_fma :: Double -> Double -> Double -> Double
foreign import ccall "math.h ldexp" c_ldexp :: Double -> CInt -> Double
foreign import ccall "stdlib.h strtoul" c_strtoul :: CString -> Ptr CString -> CInt -> IO CULong

main :: IO ()
main = do
  n <- withCString "twelve chars" c_strlen
  print ("newtype result", n)
  allocaBytes 16 $ \p -> do
    q <- c_memset p 65 8
    xs <- peekArray 8 (castPtr p :: Ptr Word8)
    print ("memset returns its argument", q == p, xs)
    same <- c_memcmp p q 8
    print ("memcmp", same)
  allocaBytes 32 $ \buf -> withCString "%d-%d" $ \fmt -> do
    written <- c_snprintf5 buf 32 fmt 12 (-34)
    s <- peekCString buf
    print ("five arguments", written, s)
  allocaBytes 8 $ \buf -> withCString "abcdefghij" $ \src -> do
    _ <- c_memset buf 0 8
    r <- c_strncpy buf src 3
    s <- peekCString r
    print ("type synonym for a pointer", s)
  print ("sized integers", c_llabs (-9000000000), c_abs32 (-5), c_toupper 97)
  r <- c_close (Fd (-1))
  print ("newtype argument", r)
  print ("three doubles, and a double with an int", c_fma 2 3 4, c_ldexp 1.5 4)
  v <- withCString "4294967296 rest" $ \s -> alloca $ \end -> do
    x <- c_strtoul s end 10
    e <- peek end
    rest <- peekCString e
    return (x, rest)
  print ("unsigned long and a pointer to a pointer", v)
