-- Haskell 2010 Report 38.2 (Numeric): readFloat, lexDigits, readSigned
-- (only complete numeric lexemes are used; inputs like "5." or "1e" are in the -ghc-differs file)
module Main (main) where

import Data.Ratio
import Numeric

flt :: String -> [(Double, String)]
flt = readFloat

sdec :: String -> [(Int, String)]
sdec = readSigned readDec

sflt :: String -> [(Double, String)]
sflt = readSigned readFloat

main :: IO ()
main = do
  print ("readFloat", flt "2.5")
  print ("readFloat integer syntax", flt "3")
  print ("readFloat exponent", flt "1e3")
  print ("readFloat fraction and exponent", flt "1.5e2")
  print ("readFloat negative exponent", flt "1.5e-2")
  print ("readFloat explicit positive exponent", flt "1.5E+2")
  print ("readFloat upper case E", flt "25E1")
  print ("readFloat leading zeros", flt "0.001", flt "000.5")
  print ("readFloat many digits", flt "3.141592653589793")
  print ("readFloat rest", flt "2.5 rest")
  print ("readFloat rest after exponent", flt "12.5e1 rest")
  print ("readFloat tenth", flt "0.1", fmap fst (flt "0.1") == [0.1])
  print ("readFloat no digits", flt "abc")
  print ("readFloat empty", flt "")
  print ("readFloat no leading dot", flt ".5")
  print ("readFloat no sign", flt "-2.5")
  print ("readFloat Float", (readFloat "0.1" :: [(Float, String)]))
  print ("readFloat Rational is exact", (readFloat "0.75" :: [(Rational, String)]), (readFloat "0.1" :: [(Rational, String)]))
  print ("readFloat Rational exponent", (readFloat "1.25e2" :: [(Rational, String)]), (readFloat "5e-3" :: [(Rational, String)]))
  print ("readFloat large", flt "1e100", flt "123456789012345678901234567890")
  print ("lexDigits", lexDigits "123abc")
  print ("lexDigits all", lexDigits "2010")
  print ("lexDigits keeps leading zeros", lexDigits "007 ")
  print ("lexDigits no digits", lexDigits "abc")
  print ("lexDigits empty", lexDigits "")
  print ("lexDigits no leading space", lexDigits " 1")
  print ("lexDigits stops at dot", lexDigits "3.14")
  print ("readSigned positive", sdec "17")
  print ("readSigned negative", sdec "-42")
  print ("readSigned rest", sdec "-42 rest")
  print ("readSigned skips leading space", sdec "  -42 rest", sdec "  7")
  print ("readSigned space after minus", sdec "- 5")
  print ("readSigned parenthesised", sdec "(5)", sdec "(-5)", sdec " ( -5 ) x")
  print ("readSigned no digits", sdec "abc", sdec "-", sdec "")
  print ("readSigned double minus", sdec "--5")
  print ("readSigned readFloat", sflt "-2.5", sflt "2.5", sflt "-1.5e2 x")
  print ("readSigned readFloat integer syntax", sflt "-3")
  print ("readSigned readHex", (readSigned readHex "-ff" :: [(Int, String)]))
  print ("readSigned Integer", (readSigned readDec "-123456789012345678901234567890" :: [(Integer, String)]))
  print ("readSigned . showSigned", all (\n -> sdec (showSigned showInt 0 n "") == [(n, "")]) [-100, -1, 0, 1, 100])
  print ("readSigned . showSigned parenthesised", all (\n -> sdec (showSigned showInt 11 n "") == [(n, "")]) [-100, -1, 0, 1, 100])
