-- Haskell 2010 Report 16 (Data.Char): isControl, isSpace, isLower, isUpper, isAlpha, isLetter, isDigit,
-- isOctDigit, isHexDigit, isAlphaNum, isPrint tabulated over all of ASCII
module Main (main) where

import Data.Char

ascii :: String
ascii = ['\0' .. '\127']

table :: String -> (Char -> Bool) -> IO ()
table name p = do
  putStrLn (name ++ " count: " ++ show (length (filter p ascii)))
  putStrLn (name ++ " codes: " ++ show (map ord (filter p ascii)))

main :: IO ()
main = do
  table "isControl" isControl
  table "isSpace" isSpace
  table "isLower" isLower
  table "isUpper" isUpper
  table "isAlpha" isAlpha
  table "isLetter" isLetter
  table "isDigit" isDigit
  table "isOctDigit" isOctDigit
  table "isHexDigit" isHexDigit
  table "isAlphaNum" isAlphaNum
  table "isPrint" isPrint
  print ("isSpace chars", filter isSpace ascii)
  print ("isLower chars", filter isLower ascii)
  print ("isUpper chars", filter isUpper ascii)
  print ("isDigit chars", filter isDigit ascii)
  print ("isOctDigit chars", filter isOctDigit ascii)
  print ("isHexDigit chars", filter isHexDigit ascii)
  print ("isAlphaNum chars", filter isAlphaNum ascii)
  print ("printable non alphanumeric", filter (\c -> isPrint c && not (isAlphaNum c)) ascii)
  print ("isAlpha equals isLetter", all (\c -> isAlpha c == isLetter c) ascii)
  print ("isAlpha is lower or upper", all (\c -> isAlpha c == (isLower c || isUpper c)) ascii)
  print ("isAlphaNum is alpha or digit", all (\c -> isAlphaNum c == (isAlpha c || isDigit c)) ascii)
  print ("control and print are disjoint and total", all (\c -> isControl c /= isPrint c) ascii)
  print ("boundaries", isDigit '/', isDigit ':', isUpper '@', isUpper '[', isLower '`', isLower '{')
  print ("hex boundaries", isHexDigit 'f', isHexDigit 'g', isHexDigit 'F', isHexDigit 'G')
  print ("oct boundaries", isOctDigit '7', isOctDigit '8')
