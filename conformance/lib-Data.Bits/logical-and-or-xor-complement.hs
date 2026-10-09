-- Haskell 2010 Report 15 (Data.Bits): (.&.), (.|.), xor, complement at Int, and the operator fixities
module Main (main) where

import Data.Bits

a :: Int
a = 12

b :: Int
b = 10

main :: IO ()
main = do
  print ("and", a .&. b)
  print ("or", a .|. b)
  print ("xor", a `xor` b)
  print ("complement", complement a)
  print ("complement zero", complement (0 :: Int))
  print ("complement minus one", complement (-1 :: Int))
  print ("complement twice", complement (complement a) == a)
  print ("and with negative", (-1) .&. a, (-4) .&. 7 :: Int)
  print ("or with negative", (-8) .|. 3 :: Int)
  print ("xor with negative", (-1) `xor` a, (-6) `xor` (-3) :: Int)
  print ("and zero", a .&. 0)
  print ("or zero", a .|. 0)
  print ("xor self", a `xor` a)
  print ("minBound and maxBound", (minBound :: Int) .&. maxBound, (minBound :: Int) .|. maxBound)
  print ("minBound xor maxBound", (minBound :: Int) `xor` maxBound)
  print ("complement maxBound", complement (maxBound :: Int) == minBound)
  print ("de morgan", complement (a .&. b) == complement a .|. complement b)
  print ("xor via and or", a `xor` b == (a .|. b) .&. complement (a .&. b))
  print ("negate is complement plus one", negate a == complement a + 1)
  -- infixl 7 .&. ; infixl 6 `xor` ; infixl 5 .|.
  print ("and binds tighter than or", 1 .|. 2 .&. 4 :: Int)
  print ("and binds tighter than xor", 6 `xor` 3 .&. 5 :: Int)
  print ("xor binds tighter than or", 5 .|. 2 `xor` 7 :: Int)
  print ("or is left associative", 1 .|. 2 .|. 4 :: Int)
  print ("and vs comparison", 6 .&. 3 == (2 :: Int))
  print ("and vs addition", 1 + 2 .&. 3 + 4 :: Int)
  print ("and vs multiplication is left to right", 2 * 3 .&. 5 :: Int, 5 .&. 3 * 2 :: Int)
  print ("isSigned Int", isSigned a)
