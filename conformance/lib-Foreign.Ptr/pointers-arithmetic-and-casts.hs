-- Haskell 2010 Report 34 (Foreign.Ptr): nullPtr, castPtr, plusPtr, alignPtr, minusPtr; FunPtr; IntPtr and
-- WordPtr. Only differences and comparisons are printed, never an address the system chose.
module Main (main) where

import Foreign.Marshal.Alloc
import Foreign.Ptr

main :: IO ()
main = do
  print ("null", nullPtr == (nullPtr :: Ptr Int), (nullPtr :: Ptr ()) == castPtr (nullPtr :: Ptr Char))
  print ("show null", show (nullPtr :: Ptr Int), show (nullFunPtr :: FunPtr (IO ())))
  print ("show small", show (nullPtr `plusPtr` 255 :: Ptr ()), show (nullPtr `plusPtr` 4096 :: Ptr ()))
  p <- mallocBytes 64 :: IO (Ptr ())
  let q = p `plusPtr` 10 :: Ptr ()
  print ("plus and minus", q `minusPtr` p, p `minusPtr` q, (q `plusPtr` (-10)) == p)
  print ("order", p < q, compare q p, max p q == q, p == p)
  let a = nullPtr `plusPtr` 13 :: Ptr ()
  print ("align", alignPtr a 8 `minusPtr` nullPtr, alignPtr a 4 `minusPtr` nullPtr, alignPtr (nullPtr `plusPtr` 16 :: Ptr ()) 8 `minusPtr` nullPtr)
  print ("cast keeps the address", castPtr p == (castPtr (castPtr p :: Ptr Int) :: Ptr ()))
  print ("IntPtr", ptrToIntPtr (nullPtr `plusPtr` 77 :: Ptr ()), intPtrToPtr 77 == (nullPtr `plusPtr` 77 :: Ptr ()))
  print ("WordPtr", ptrToWordPtr (nullPtr `plusPtr` 99 :: Ptr ()), wordPtrToPtr 99 == (nullPtr `plusPtr` 99 :: Ptr ()))
  print ("round trips", intPtrToPtr (ptrToIntPtr p) == p, wordPtrToPtr (ptrToWordPtr q) == q)
  print ("IntPtr is a number", ptrToIntPtr q - ptrToIntPtr p, (5 :: IntPtr) + 6, (7 :: WordPtr) * 6)
  print ("FunPtr", nullFunPtr == (nullFunPtr :: FunPtr (Int -> Int)), castFunPtrToPtr (nullFunPtr :: FunPtr ()) == (nullPtr :: Ptr ()))
  print ("FunPtr casts", castPtrToFunPtr (nullPtr :: Ptr ()) == (nullFunPtr :: FunPtr ()), castFunPtr (nullFunPtr :: FunPtr Int) == (nullFunPtr :: FunPtr Char))
  free p
