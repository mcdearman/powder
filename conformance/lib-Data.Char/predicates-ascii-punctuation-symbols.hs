-- Haskell 2010 Report 16 (Data.Char): isPunctuation, isSymbol, isSeparator, isMark, isNumber, isAscii,
-- isLatin1, isAsciiUpper, isAsciiLower tabulated over all of ASCII
module Main (main) where

import Data.Char

ascii :: String
ascii = ['\0' .. '\127']

main :: IO ()
main = do
  print ("isPunctuation", filter isPunctuation ascii)
  print ("isSymbol", filter isSymbol ascii)
  print ("isSeparator", filter isSeparator ascii)
  print ("isMark", filter isMark ascii)
  print ("isNumber", filter isNumber ascii)
  print ("isAsciiUpper", filter isAsciiUpper ascii)
  print ("isAsciiLower", filter isAsciiLower ascii)
  print ("isAscii all", all isAscii ascii)
  print ("isLatin1 all", all isLatin1 ascii)
  print ("isAscii boundary", isAscii '\127', isAscii '\128')
  print ("isLatin1 boundary", isLatin1 '\255', isLatin1 '\256')
  print ("isAscii of large", isAscii '\1234', isAscii maxBound)
  print ("isLatin1 of large", isLatin1 '\1234', isLatin1 maxBound)
  print ("punctuation count", length (filter isPunctuation ascii))
  print ("symbol count", length (filter isSymbol ascii))
  print ("punctuation and symbol disjoint", not (any (\c -> isPunctuation c && isSymbol c) ascii))
  print ("space is separator not punctuation", isSeparator ' ', isPunctuation ' ', isSymbol ' ')
  print ("tab and newline are not separators", isSeparator '\t', isSeparator '\n')
  print ("isNumber equals isDigit on ascii", all (\c -> isNumber c == isDigit c) ascii)
  print ("isAsciiUpper equals isUpper on ascii", all (\c -> isAsciiUpper c == isUpper c) ascii)
  print ("isAsciiLower equals isLower on ascii", all (\c -> isAsciiLower c == isLower c) ascii)
  print ("isAsciiUpper non ascii", isAsciiUpper '\192', isUpper '\192')
  print ("isAsciiLower non ascii", isAsciiLower '\224', isLower '\224')
  print
    ( "printable ascii is partitioned"
    , all (\c -> length (filter ($ c) [isAlphaNum, isPunctuation, isSymbol, isSeparator]) == 1) [' ' .. '~']
    )
  print ("underscore", isPunctuation '_', isSymbol '_')
  print ("dollar", isPunctuation '$', isSymbol '$')
  print ("backquote and caret", isSymbol '`', isSymbol '^')
