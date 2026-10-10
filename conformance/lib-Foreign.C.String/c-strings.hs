-- Haskell 2010 Report 26 (Foreign.C.String): C strings, with and without a length, the character casts,
-- the CA functions that keep eight bits of each character, and wide strings
module Main (main) where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

foreign import ccall "string.h strlen" c_strlen :: CString -> IO CSize

main :: IO ()
main = do
  p <- newCString "hello"
  n <- c_strlen p
  s <- peekCString p
  bytes <- peekArray 6 p
  print ("newCString", n, s, bytes)
  free p
  t <- withCString "with it" peekCString
  l <- withCString "" c_strlen
  print ("withCString", t, l)
  (q, len) <- newCStringLen "no zero"
  u <- peekCStringLen (q, len)
  v <- peekCStringLen (q, 2)
  print ("with a length", len, u, v)
  free q
  w <- withCStringLen "abc" (\(r, k) -> peekArray k r >>= \cs -> return (k, cs))
  print ("withCStringLen", w)
  print ("casts", castCharToCChar 'A', castCCharToChar 66, castCharToCUChar 'z', castCUCharToChar 200, castCharToCSChar 'a', castCSCharToChar 98)
  print ("casts keep eight bits", castCCharToChar (castCharToCChar '\233'), castCUCharToChar (castCharToCUChar '\255'))
  a <- newCAString "plain ascii"
  b <- peekCAString a
  c <- peekCAStringLen (a, 5)
  print ("CA strings", b, c)
  free a
  d <- withCAString "xyz" (\r -> c_strlen r)
  e <- withCAStringLen "xyz" (\(r, k) -> peekCAStringLen (r `plusPtr` 1, k - 1))
  (f, flen) <- newCAStringLen "lengthy"
  print ("more CA strings", d, e, flen)
  free f
  ok <- charIsRepresentable 'a'
  print ("charIsRepresentable", ok)
  wp <- newCWString "wide \955"
  ws <- peekCWString wp
  wcs <- peekArray 7 wp
  print ("wide strings", ws, wcs)
  free wp
  x <- withCWString "abc" (\r -> peekCWStringLen (r, 2))
  y <- withCWStringLen "hello" (\(r, k) -> peekElemOff r (k - 1) >>= \c' -> return (k, c'))
  (zp, zl) <- newCWStringLen "four"
  z <- peekCWStringLen (zp, zl)
  print ("more wide strings", x, y, zl, z)
  free zp
