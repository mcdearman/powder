-- Haskell 2010 Report 36 (Foreign.StablePtr): a value kept for C to hand back, of any type
module Main (main) where

import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable

main :: IO ()
main = do
  s <- newStablePtr "a string"
  f <- newStablePtr (+ (1 :: Int))
  l <- newStablePtr [1 :: Integer ..]
  x <- deRefStablePtr s
  g <- deRefStablePtr f
  ys <- deRefStablePtr l
  print ("values come back", x, g 41, take 3 ys)
  again <- deRefStablePtr s
  print ("as often as asked", again == x)
  print ("equality", s == s)
  let p = castStablePtrToPtr s
  print ("as a pointer it is not null", p /= nullPtr)
  y <- deRefStablePtr (castPtrToStablePtr p :: StablePtr String)
  print ("through a pointer and back", y)
  z <- alloca $ \cell -> do
    poke cell s
    s' <- peek cell
    deRefStablePtr s'
  print ("kept in memory", z)
  lazy <- newStablePtr (undefined :: Int)
  print "a value is not looked at to be kept"
  freeStablePtr lazy
  freeStablePtr s
  freeStablePtr f
  freeStablePtr l
  putStrLn "freed"
