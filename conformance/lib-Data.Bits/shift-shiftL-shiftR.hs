-- Haskell 2010 Report 15 (Data.Bits): shift, shiftL, shiftR at Int (arithmetic right shift; negative
-- amounts via shift only), and their fixity (infixl 8)
module Main (main) where

import Data.Bits

main :: IO ()
main = do
  print ("shiftL", (1 :: Int) `shiftL` 4)
  print ("shiftL zero", (5 :: Int) `shiftL` 0)
  print ("shiftL 62", (1 :: Int) `shiftL` 62)
  print ("shiftL 63 is minBound", (1 :: Int) `shiftL` 63 == minBound)
  print ("shiftL overflow drops bits", (3 :: Int) `shiftL` 63 == minBound)
  print ("shiftL negative value", (-3 :: Int) `shiftL` 2)
  print ("shiftR", (256 :: Int) `shiftR` 4)
  print ("shiftR zero", (5 :: Int) `shiftR` 0)
  print ("shiftR drops low bits", (7 :: Int) `shiftR` 1)
  print ("shiftR to zero", (7 :: Int) `shiftR` 3)
  print ("shiftR negative is arithmetic", (-8 :: Int) `shiftR` 1)
  print ("shiftR negative rounds down", (-7 :: Int) `shiftR` 1)
  print ("shiftR minus one", (-1 :: Int) `shiftR` 10)
  print ("shiftR minBound 63", (minBound :: Int) `shiftR` 63)
  print ("shiftR maxBound 62", (maxBound :: Int) `shiftR` 62)
  print ("shift positive is left", shift (3 :: Int) 2)
  print ("shift negative is right", shift (12 :: Int) (-2))
  print ("shift zero", shift (12 :: Int) 0)
  print ("shift negative value right", shift (-12 :: Int) (-2))
  print ("shift agrees with shiftL", shift (5 :: Int) 3 == (5 :: Int) `shiftL` 3)
  print ("shift agrees with shiftR", shift (-40 :: Int) (-3) == (-40 :: Int) `shiftR` 3)
  print ("shiftL is multiplication", [(7 :: Int) `shiftL` k == 7 * 2 ^ k | k <- [0 .. 10 :: Int]])
  print ("shiftR is div", [(-1000 :: Int) `shiftR` k == (-1000) `div` 2 ^ k | k <- [0 .. 12 :: Int]])
  -- infixl 8 `shift`, `shiftL`, `shiftR`
  print ("shift binds tighter than plus", 1 `shiftL` 2 + 1 :: Int)
  print ("shift binds tighter than times", 3 * 1 `shiftL` 2 :: Int)
  print ("shift binds tighter than and", 1 `shiftL` 3 .|. 1 `shiftL` 1 :: Int)
  print ("shift is left associative", 1 `shiftL` 2 `shiftL` 3 :: Int)
  print ("left then right", 5 `shiftL` 4 `shiftR` 2 :: Int)
  print ("shift binds tighter than comparison", 1 `shiftL` 2 == (4 :: Int))
