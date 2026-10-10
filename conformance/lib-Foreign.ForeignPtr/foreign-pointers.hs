-- Haskell 2010 Report 30 (Foreign.ForeignPtr): pointers with finalizers; malloc'd foreign pointers. A
-- finalizer here is C's free, whose running cannot be seen; what is checked is that the memory is there
-- to use and that finalizing does no harm.
module Main (main) where

import Data.Int
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

main :: IO ()
main = do
  fp <- mallocForeignPtr :: IO (ForeignPtr Int32)
  x <- withForeignPtr fp (\p -> poke p 42 >> peek p)
  print ("mallocForeignPtr", x)
  fb <- mallocForeignPtrBytes 16 :: IO (ForeignPtr Int64)
  y <- withForeignPtr fb (\p -> pokeElemOff p 1 7 >> peekElemOff p 1)
  print ("mallocForeignPtrBytes", y)
  fa <- mallocForeignPtrArray 4 :: IO (ForeignPtr Int32)
  ys <- withForeignPtr fa (\p -> pokeArray p [1, 2, 3, 4] >> peekArray 4 p)
  print ("mallocForeignPtrArray", ys)
  fz <- mallocForeignPtrArray0 3 :: IO (ForeignPtr Int32)
  zs <- withForeignPtr fz (\p -> pokeArray0 0 p [5, 6, 7] >> peekArray0 0 p)
  print ("mallocForeignPtrArray0", zs)
  raw <- malloc :: IO (Ptr Int32)
  poke raw 99
  fr <- newForeignPtr finalizerFree raw
  r <- withForeignPtr fr peek
  print ("newForeignPtr with free", r)
  print ("equality and order", fp == fp, fp == castForeignPtr fb, compare fr fr)
  plain <- newForeignPtr_ nullPtr :: IO (ForeignPtr ())
  print ("newForeignPtr_ of null", withNull plain)
  other <- malloc :: IO (Ptr Int32)
  fo <- newForeignPtr_ other
  addForeignPtrFinalizer finalizerFree fo
  touchForeignPtr fo
  finalizeForeignPtr fo
  putStrLn "finalized early"
  finalizeForeignPtr fo
  putStrLn "finalizing again does nothing"
  w <- withForeignPtr (castForeignPtr fp :: ForeignPtr Int8) peek
  print ("castForeignPtr", w)
  where
    withNull p = p == p
