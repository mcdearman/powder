module Data.Char
  ( isControl, isSpace, isLower, isUpper, isAlpha, isDigit, isOctDigit, isHexDigit, isAlphaNum, isPunctuation,
    isAscii, isLatin1, isAsciiUpper, isAsciiLower, isPrint, isLetter, isMark, isNumber, isSymbol, isSeparator,
    GeneralCategory (..), generalCategory, toUpper, toLower, toTitle, digitToInt, intToDigit, ord, chr,
    lexLitChar, readLitChar, showLitChar
  )
where

import Data.Ix
import Powder.Base (category, chr, digitToInt, isAlpha, isAlphaNum, isDigit, isHexDigit, isLower, isOctDigit, isSpace, isUpper, lexLitChar, ord, readLitChar, showLitChar, toLower, toUpper)

-- Unicode's general categories, in the order the standard lists them.
data GeneralCategory
  = UppercaseLetter
  | LowercaseLetter
  | TitlecaseLetter
  | ModifierLetter
  | OtherLetter
  | NonSpacingMark
  | SpacingCombiningMark
  | EnclosingMark
  | DecimalNumber
  | LetterNumber
  | OtherNumber
  | ConnectorPunctuation
  | DashPunctuation
  | OpenPunctuation
  | ClosePunctuation
  | InitialQuote
  | FinalQuote
  | OtherPunctuation
  | MathSymbol
  | CurrencySymbol
  | ModifierSymbol
  | OtherSymbol
  | Space
  | LineSeparator
  | ParagraphSeparator
  | Control
  | Format
  | Surrogate
  | PrivateUse
  | NotAssigned
  deriving (Show, Eq, Ord, Enum, Bounded, Read, Ix)

generalCategory :: Char -> GeneralCategory
generalCategory c = toEnum (fromInteger (category c))

-- Whether a character's category is between two, by their places.
within :: Integer -> Integer -> Char -> Bool
within lo hi c = let k = category c in k >= lo && k <= hi

isControl, isPrint, isLetter, isMark, isNumber, isPunctuation, isSymbol, isSeparator :: Char -> Bool
isControl = within 25 25
isPrint c = category c <= 22
isLetter = within 0 4
isMark = within 5 7
isNumber = within 8 10
isPunctuation = within 11 17
isSymbol = within 18 21
isSeparator = within 22 24

isAscii, isLatin1, isAsciiUpper, isAsciiLower :: Char -> Bool
isAscii c = ord c < 128
isLatin1 c = ord c < 256
isAsciiUpper c = c >= 'A' && c <= 'Z'
isAsciiLower c = c >= 'a' && c <= 'z'

toTitle :: Char -> Char
toTitle c = primIntToChar (primCharTitle (primCharToInt c))

intToDigit :: Int -> Char
intToDigit n
  | n >= 0 && n < 10 = chr (ord '0' + n)
  | n >= 10 && n < 16 = chr (ord 'a' + n - 10)
  | otherwise = error "Char.intToDigit: not a digit"
