-- Haskell 2010 Report 33 (Foreign.Marshal.Utils): with, new, fromBool, toBool, maybeNew, maybeWith,
-- maybePeek, withMany, copyBytes, moveBytes
module Main (main) where

import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

main :: IO ()
main = do
  x <- with (42 :: Int32) peek
  y <- with 'q' (\p -> poke p 'r' >> peek p)
  print ("with", x, y)
  p <- new (2.5 :: Double)
  z <- peek p
  print ("new", z)
  print ("fromBool", fromBool True :: CInt, fromBool False :: Int, fromBool True :: Double)
  print ("toBool", toBool (0 :: CInt), toBool (1 :: CInt), toBool (-5 :: Int), toBool (0.0 :: Double))
  nothing <- maybeNew new (Nothing :: Maybe Int32)
  just <- maybeNew new (Just (7 :: Int32))
  v <- peek just
  print ("maybeNew", nothing == nullPtr, just /= nullPtr, v)
  a <- maybePeek peek nothing
  b <- maybePeek peek just
  print ("maybePeek", a, b)
  c <- maybeWith with (Nothing :: Maybe Int32) (\q -> return (q == nullPtr))
  d <- maybeWith with (Just (9 :: Int32)) peek
  print ("maybeWith", c, d)
  e <- withMany with [1, 2, 3 :: Int32] (mapM peek)
  f <- withMany (\s k -> k (length s)) ["a", "bb", "ccc"] return
  print ("withMany", e, f)
  src <- newArray [1 .. 8 :: Word8]
  dst <- mallocBytes 8 :: IO (Ptr Word8)
  copyBytes dst src 8
  g <- peekArray 8 dst
  moveBytes (dst `plusPtr` 2) dst 6
  h <- peekArray 8 dst
  print ("copyBytes and moveBytes", g, h)
  mapM_ free [castPtr p :: Ptr (), castPtr just, castPtr src, castPtr dst]
