module Numeric
  ( showSigned, showIntAtBase, showInt, showHex, showOct, readSigned, readInt, readDec, readOct, readHex, readFloat, lexDigits
  )
where

import Data.Char (intToDigit)
import Powder.Base (lexDigits, readDec, readFloat, readHex, readInt, readOct, readSigned)

showSigned :: Real a => (a -> ShowS) -> Int -> a -> ShowS
showSigned showPos p x
  | x < 0 = showParen (p > 6) (showChar '-' . showPos (negate x))
  | otherwise = showPos x

showIntAtBase :: Integral a => a -> (Int -> Char) -> a -> ShowS
showIntAtBase base digit n rest
  | n < 0 = error "Numeric.showIntAtBase: negative number"
  | n < base = digit (fromIntegral n) : rest
  | otherwise = showIntAtBase base digit (quot n base) (digit (fromIntegral (rem n base)) : rest)

showInt, showHex, showOct :: Integral a => a -> ShowS
showInt = showIntAtBase 10 intToDigit
showHex = showIntAtBase 16 intToDigit
showOct = showIntAtBase 8 intToDigit
