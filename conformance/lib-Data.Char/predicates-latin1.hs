-- Haskell 2010 Report 16 (Data.Char): the character predicates on selected Latin-1 code points
-- (only code points whose classification has long been stable are used; 167, 170, 182, 186 are avoided)
module Main (main) where

import Data.Char

samples :: String
samples =
  map
    chr
    [133, 159, 160, 161, 162, 168, 169, 171, 172, 173, 176, 177, 178, 181, 183, 187, 188, 191, 192, 214, 215, 216, 222, 223, 224, 246, 247, 248, 255]

table :: String -> (Char -> Bool) -> IO ()
table name p = putStrLn (name ++ ": " ++ show (map ord (filter p samples)))

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
  table "isMark" isMark
  table "isNumber" isNumber
  table "isPunctuation" isPunctuation
  table "isSymbol" isSymbol
  table "isSeparator" isSeparator
  table "isAscii" isAscii
  table "isLatin1" isLatin1
  table "isAsciiUpper" isAsciiUpper
  table "isAsciiLower" isAsciiLower
  print ("C1 controls", map ord (filter isControl ['\128' .. '\255']))
  print ("latin1 spaces", map ord (filter isSpace ['\0' .. '\255']))
  print ("latin1 upper", map ord (filter isUpper ['\128' .. '\255']))
  print ("latin1 separators", map ord (filter isSeparator ['\0' .. '\255']))
  print ("beyond latin1: combining acute is a mark", isMark '\769', isAlpha '\769')
  print ("beyond latin1: greek alpha", isLower '\945', isUpper '\913', isAlpha '\945')
  print ("beyond latin1: arabic-indic digit", isDigit '\1632', isNumber '\1632', isAlphaNum '\1632')
  print ("beyond latin1: roman numeral one", isNumber '\8544', isDigit '\8544', isAlpha '\8544')
  print ("beyond latin1: line separator", isSeparator '\8232', isPunctuation '\8232')
  print ("beyond latin1: em space", isSeparator '\8195', isSpace '\8195')
