-- Haskell 2010 Report 24, 29 (Foreign, Foreign.C, Foreign.Marshal): the modules that bring the others in
module Main (main) where

import Foreign
import Foreign.C

foreign import ccall "string.h strlen" c_strlen :: CString -> IO CSize

main :: IO ()
main = do
  -- Data.Bits, Data.Int and Data.Word come with Foreign
  print ((0xF0 :: Word8) .&. 0x3C, shiftL (1 :: Int32) 4, popCountish (7 :: Word16))
  -- and Ptr, Storable and the marshalling libraries
  n <- with (5 :: Int64) (\p -> peek p >>= \x -> poke p (x * 2) >> peek p)
  xs <- withArray [1, 2, 3 :: Int16] (peekArray 3)
  print (n, xs, nullPtr == (nullPtr :: Ptr ()), sizeOf (0 :: Word32))
  s <- newStablePtr 'k'
  c <- deRefStablePtr s
  freeStablePtr s
  fp <- mallocForeignPtr :: IO (ForeignPtr Word8)
  b <- withForeignPtr fp (\p -> poke p 200 >> peek p)
  print (c, b)
  -- Foreign.C has the types, the strings and errno
  l <- withCString "from Foreign.C" c_strlen
  e <- getErrno
  print (l, (3 :: CInt) + 4, isValidErrno e)
  where
    popCountish w = length (filter (testBit w) [0 .. 15])
