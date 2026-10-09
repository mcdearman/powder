module Data.Char
  ( isControl, isSpace, isLower, isUpper, isAlpha, isDigit, isOctDigit, isHexDigit, isAlphaNum, isPunctuation,
    isAscii, isLatin1, isAsciiUpper, isAsciiLower, isPrint, isLetter, isMark, isNumber, isSymbol, isSeparator, toTitle,
    toUpper, toLower, digitToInt, intToDigit, ord, chr, lexLitChar, readLitChar, showLitChar
  )
where

import Powder.Base (chr, digitToInt, isAlpha, isAlphaNum, isDigit, isHexDigit, isLower, isOctDigit, isSpace, isUpper, lexLitChar, ord, readLitChar, showLitChar, toLower, toUpper)

isControl :: Char -> Bool
isControl c = ord c < 32 || ord c == 127

isPunctuation :: Char -> Bool
isPunctuation c = c `elem` "!\"#%&'()*,-./:;?@[\\]_{}"

isAscii, isLatin1, isAsciiUpper, isAsciiLower, isPrint :: Char -> Bool
isAscii c = ord c < 128
isLatin1 c = ord c < 256
isAsciiUpper = isUpper
isAsciiLower = isLower
isPrint c = not (isControl c)

-- These know the first 256 characters only.
isLetter, isMark, isNumber, isSymbol, isSeparator :: Char -> Bool
isLetter = isAlpha
isMark _ = False
isNumber c = isDigit c || ord c `elem` [178, 179, 185, 188, 189, 190]
isSymbol c = c `elem` "$+<=>^`|~" || ord c `elem` [162, 163, 164, 165, 166, 168, 169, 172, 174, 175, 176, 177, 180, 184, 215, 247]
isSeparator c = c == ' ' || ord c == 160

toTitle :: Char -> Char
toTitle = toUpper

intToDigit :: Int -> Char
intToDigit n
  | n >= 0 && n < 10 = chr (ord '0' + n)
  | n >= 10 && n < 16 = chr (ord 'a' + n - 10)
  | otherwise = error "Char.intToDigit: not a digit"
