-- Haskell 2010 Report 16 (Data.Char): digitToInt, intToDigit, ord, chr
module Main (main) where

import Data.Char

toBase :: Int -> Int -> String
toBase b 0 = "0"
toBase b n = reverse (go n)
  where
    go 0 = ""
    go k = intToDigit (k `mod` b) : go (k `div` b)

fromBase :: Int -> String -> Int
fromBase b = foldl (\acc c -> acc * b + digitToInt c) 0

main :: IO ()
main = do
  print ("digitToInt decimal", map digitToInt "0123456789")
  print ("digitToInt lower hex", map digitToInt "abcdef")
  print ("digitToInt upper hex", map digitToInt "ABCDEF")
  print ("intToDigit", map intToDigit [0 .. 15])
  print ("round trip", map (digitToInt . intToDigit) [0 .. 15] == [0 .. 15])
  print ("intToDigit . digitToInt lower cases", map (intToDigit . digitToInt) "09afAF")
  print ("toBase 2", toBase 2 10, toBase 2 255)
  print ("toBase 16", toBase 16 255, toBase 16 48879)
  print ("toBase 8", toBase 8 0, toBase 8 64)
  print ("fromBase", fromBase 16 "ff", fromBase 16 "BEEF", fromBase 2 "1010", fromBase 8 "777")
  print ("ord", ord 'a', ord 'A', ord '0', ord ' ', ord '\n')
  print ("ord extremes", ord '\0', ord (minBound :: Char), ord (maxBound :: Char))
  print ("ord non ascii", ord '\255', ord '\x20AC', ord '\x10000')
  print ("chr", chr 97, chr 65, chr 48, chr 32)
  print ("chr control", chr 0, chr 10, chr 127)
  print ("chr non ascii", chr 255 == '\255', chr 8364 == '\8364', chr 1114111 == maxBound)
  print ("ord . chr", map (ord . chr) [0, 127, 128, 255, 256, 65535, 65536, 1114111])
  print ("chr . ord", all (\c -> chr (ord c) == c) ['\0' .. '\300'])
  print ("ord is fromEnum", all (\c -> ord c == fromEnum c) "aZ09 ~")
  print ("chr is toEnum", chr 100 == (toEnum 100 :: Char))
  print ("ord is monotone", ord 'a' < ord 'b', ord 'Z' < ord 'a', ord '9' < ord 'A')
  print ("caesar", map (\c -> chr ((ord c - ord 'a' + 13) `mod` 26 + ord 'a')) "hello")
