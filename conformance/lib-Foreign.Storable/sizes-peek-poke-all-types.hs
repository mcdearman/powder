-- Haskell 2010 Report 37 (Foreign.Storable): sizeOf and alignment, peek and poke, element and byte offsets,
-- for the basic foreign types
module Main (main) where

import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

roundTrip :: (Storable a, Eq a) => a -> IO Bool
roundTrip x = alloca (\p -> poke p x >> peek p >>= \y -> return (x == y))

main :: IO ()
main = do
  print ("sizes of sized ints", sizeOf (0 :: Int8), sizeOf (0 :: Int16), sizeOf (0 :: Int32), sizeOf (0 :: Int64))
  print ("sizes of sized words", sizeOf (0 :: Word8), sizeOf (0 :: Word16), sizeOf (0 :: Word32), sizeOf (0 :: Word64))
  print ("sizes of C types", sizeOf (0 :: CChar), sizeOf (0 :: CShort), sizeOf (0 :: CInt), sizeOf (0 :: CLLong), sizeOf (0 :: CFloat), sizeOf (0 :: CDouble))
  print ("sizes of others", sizeOf 'c', sizeOf (0 :: Double), sizeOf (0 :: Float), sizeOf (nullPtr :: Ptr ()) == sizeOf (0 :: CSize))
  print ("alignments", alignment (0 :: Int8), alignment (0 :: Int16), alignment (0 :: Int32), alignment (0 :: Double), alignment (0 :: CChar))
  a <- roundTrip (minBound :: Int8)
  b <- roundTrip (maxBound :: Word16)
  c <- roundTrip (minBound :: Int32)
  d <- roundTrip (maxBound :: Word64)
  e <- roundTrip (minBound :: Int64)
  print ("round trips of integers", a, b, c, d, e)
  f <- roundTrip (1.5e300 :: Double)
  g <- roundTrip (0.1 :: Float)
  h <- roundTrip 'x'
  i <- roundTrip '\1114111'
  j <- roundTrip True
  k <- roundTrip (nullPtr `plusPtr` 12345 :: Ptr Int)
  print ("round trips of the rest", f, g, h, i, j, k)
  l <- roundTrip (-7 :: CInt)
  m <- roundTrip (255 :: CUChar)
  n <- roundTrip (2.5 :: CDouble)
  o <- roundTrip (maxBound :: CULong)
  print ("round trips of C types", l, m, n, o)
  allocaBytes 32 $ \p -> do
    pokeElemOff p 0 (10 :: Int32)
    pokeElemOff p 1 (20 :: Int32)
    pokeElemOff p 2 (-30 :: Int32)
    x <- peekElemOff p 1 :: IO Int32
    y <- peekByteOff p 8 :: IO Int32
    z <- peek (p `plusPtr` 4) :: IO Int32
    print ("element and byte offsets", x, y, z)
    pokeByteOff p 12 (0x0102 :: Word16)
    lo <- peekByteOff p 12 :: IO Word8
    hi <- peekByteOff p 13 :: IO Word8
    print ("bytes of a 16-bit word, one of them 1 and the other 2", lo + hi, lo * hi)
    poke (castPtr p) (-1 :: Int8)
    u <- peek (castPtr p) :: IO Word8
    print ("the same byte signed and unsigned", u)
    poke (castPtr p) (300.75 :: Double)
    v <- peek (castPtr p) :: IO Double
    print ("double", v)
