-- Haskell 2010 Report 31 (Foreign.Marshal.Alloc): malloc, mallocBytes, alloca, allocaBytes, realloc,
-- reallocBytes, free
module Main (main) where

import Data.Int
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.IO.Error

main :: IO ()
main = do
  p <- malloc :: IO (Ptr Int64)
  poke p 123456789012345
  x <- peek p
  print ("malloc", x, p /= nullPtr)
  q <- mallocBytes 4 :: IO (Ptr Word8)
  mapM_ (\i -> pokeByteOff q i (fromIntegral (i * 10) :: Word8)) [0 .. 3]
  bigger <- reallocBytes q 1024
  kept <- mapM (\i -> peekByteOff bigger i :: IO Word8) [0 .. 3]
  pokeByteOff bigger 1023 (255 :: Word8)
  end <- peekByteOff bigger 1023 :: IO Word8
  print ("reallocBytes keeps what was there", kept, end)
  wide <- realloc p :: IO (Ptr Int64)
  y <- peek wide
  print ("realloc keeps what was there", y)
  r <- alloca (\cell -> poke cell (7 :: Int32) >> peek cell >>= \v -> return (v * 6))
  print ("alloca gives the action's result", r)
  s <- allocaBytes 12 (\buf -> mapM_ (\i -> pokeByteOff buf i (fromIntegral i :: Word8)) [0 .. 11] >> mapM (\i -> peekByteOff buf i :: IO Word8) [0, 5, 11])
  print ("allocaBytes", s)
  nested <- alloca (\a -> alloca (\b -> poke a (1 :: Int32) >> poke b (2 :: Int32) >> peek a >>= \u -> peek b >>= \v -> return (u, v, a /= b)))
  print ("nested alloca", nested)
  failed <- tryIOError (alloca (\cell -> poke cell (1 :: Int32) >> ioError (userError "inside") >> return ()))
  print ("an error passes through alloca", either ioeGetErrorString (const "none") failed)
  empty <- reallocBytes bigger 0
  print ("reallocBytes to nothing frees", empty == nullPtr)
  free wide
  free nullPtr
  putStrLn "freed"
