-- Haskell 2010 Report 32 (Foreign.Marshal.Array): allocating, reading, writing, copying and measuring
-- arrays
module Main (main) where

import Data.Int
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

main :: IO ()
main = do
  p <- mallocArray 5 :: IO (Ptr Int32)
  pokeArray p [10, 20, 30, 40, 50]
  xs <- peekArray 5 p
  some <- peekArray 2 (advancePtr p 3)
  print ("pokeArray and peekArray", xs, some, peekNone)
  print ("advancePtr moves by elements", advancePtr p 3 `minusPtr` p)
  z <- mallocArray0 3 :: IO (Ptr Word8)
  pokeArray0 0 z [7, 8, 9]
  n <- lengthArray0 0 z
  ys <- peekArray0 0 z
  whole <- peekArray 4 z
  print ("zero-terminated", n, ys, whole)
  a <- newArray [1.5, 2.5, 3.5 :: Double]
  as <- peekArray 3 a
  b <- newArray0 (-1) [4, 5 :: Int16]
  bs <- peekArray0 (-1) b
  print ("newArray and newArray0", as, bs)
  s <- withArray [1, 2, 3 :: Int64] (\q -> peekArray 3 q >>= return . sum)
  t <- withArray0 0 [3, 2, 1 :: Int32] (lengthArray0 0)
  u <- withArrayLen "hello" (\len q -> peekArray len q >>= \cs -> return (len, reverse cs))
  v <- withArrayLen0 '\0' "hi" (\len q -> peekArray (len + 1) q)
  print ("withArray family", s, t, u, v)
  r <- allocaArray 3 (\q -> pokeArray q [1, 2, 3 :: Int32] >> peekArray 3 q)
  r0 <- allocaArray0 2 (\q -> pokeArray0 9 q [1, 2 :: Word16] >> peekArray 3 q)
  print ("allocaArray family", r, r0)
  grown <- reallocArray p 8
  pokeElemOff grown 7 80
  gs <- peekArray 8 grown
  print ("reallocArray keeps the elements", take 5 gs, gs !! 7)
  grown0 <- reallocArray0 z 5
  hs <- peekArray 3 grown0
  print ("reallocArray0", hs)
  dst <- mallocArray 5 :: IO (Ptr Int32)
  copyArray dst grown 5
  ds <- peekArray 5 dst
  moveArray (advancePtr dst 1) dst 4
  ms <- peekArray 5 dst
  print ("copyArray, and moveArray over itself", ds, ms)
  mapM_ free [castPtr grown :: Ptr (), castPtr grown0, castPtr a, castPtr b, castPtr dst]
  where
    peekNone = [] :: [Int32]
