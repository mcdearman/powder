-- Haskell 2010 Report 38.2 (Numeric): readInt, readDec, readOct, readHex (unsigned; no leading white space
-- is skipped; the longest prefix of digits is consumed)
module Main (main) where

import Data.Char (digitToInt, isDigit)
import Data.Word
import Numeric

dec :: String -> [(Int, String)]
dec = readDec

oct :: String -> [(Int, String)]
oct = readOct

hex :: String -> [(Int, String)]
hex = readHex

bin :: String -> [(Int, String)]
bin = readInt 2 (`elem` "01") digitToInt

base36 :: String -> [(Integer, String)]
base36 = readInt 36 (\c -> isDigit c || (c >= 'a' && c <= 'z')) (\c -> if isDigit c then digitToInt c else fromEnum c - fromEnum 'a' + 10)

main :: IO ()
main = do
  print ("readDec", dec "123")
  print ("readDec rest", dec "123abc")
  print ("readDec rest space", dec "42 43")
  print ("readDec leading zeros", dec "007")
  print ("readDec zero", dec "0")
  print ("readDec empty", dec "")
  print ("readDec no digits", dec "abc")
  print ("readDec no sign", dec "-5")
  print ("readDec no leading space", dec " 5")
  print ("readDec stops at dot", dec "3.14")
  print ("readDec Integer", (readDec "123456789012345678901234567890" :: [(Integer, String)]))
  print ("readDec Word8", (readDec "200" :: [(Word8, String)]))
  print ("readOct", oct "17")
  print ("readOct rest", oct "178")
  print ("readOct stops at 8", oct "789")
  print ("readOct no digits", oct "8")
  print ("readOct zero", oct "0")
  print ("readOct 777", oct "777")
  print ("readOct Integer", (readOct "1000000000000000000000000" :: [(Integer, String)]))
  print ("readHex", hex "ff")
  print ("readHex upper", hex "FF")
  print ("readHex mixed", hex "fFaB")
  print ("readHex rest", hex "ffg")
  print ("readHex prefix is not understood", hex "0x1F")
  print ("readHex decimal digits", hex "10")
  print ("readHex empty", hex "")
  print ("readHex no digits", hex "xyz")
  print ("readHex Integer", (readHex "ffffffffffffffffffff" :: [(Integer, String)]))
  print ("readHex beef", hex "BEEF rest")
  print ("readInt binary", bin "1011")
  print ("readInt binary rest", bin "10112")
  print ("readInt binary none", bin "2")
  print ("readInt binary empty", bin "")
  print ("readInt base 36", base36 "zz", base36 "haskell!")
  print ("readInt base 3", (readInt 3 (`elem` "012") digitToInt "2103" :: [(Int, String)]))
  print ("readInt custom digits", (readInt 2 (`elem` "ab") (\c -> if c == 'b' then 1 else 0) "babac" :: [(Int, String)]))
  print ("readDec . showInt", all (\n -> dec (showInt n "") == [(n, "")]) [0, 7, 10, 123456, maxBound])
  print ("readHex . showHex", all (\n -> hex (showHex n "") == [(n, "")]) [0, 7, 10, 255, 123456, maxBound])
  print ("readOct . showOct", all (\n -> oct (showOct n "") == [(n, "")]) [0, 7, 8, 255, 123456, maxBound])
