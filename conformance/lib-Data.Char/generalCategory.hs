-- Haskell 2010 Report 16 (Data.Char): generalCategory over ASCII and one stable code point per category
module Main (main) where

import Data.Char

ascii :: String
ascii = ['\0' .. '\127']

inCategory :: GeneralCategory -> String
inCategory cat = filter (\c -> generalCategory c == cat) ascii

main :: IO ()
main = do
  mapM_ (\cat -> print (cat, map ord (inCategory cat))) [minBound .. maxBound]
  print ("categories used by ascii", length (filter (not . null . inCategory) [minBound .. maxBound]))
  print ("UppercaseLetter", generalCategory 'A')
  print ("LowercaseLetter", generalCategory 'a')
  print ("TitlecaseLetter", generalCategory '\453')
  print ("ModifierLetter", generalCategory '\688')
  print ("OtherLetter", generalCategory '\1488')
  print ("NonSpacingMark", generalCategory '\768')
  print ("SpacingCombiningMark", generalCategory '\2307')
  print ("EnclosingMark", generalCategory '\8413')
  print ("DecimalNumber", generalCategory '0')
  print ("LetterNumber", generalCategory '\8544')
  print ("OtherNumber", generalCategory '\178')
  print ("ConnectorPunctuation", generalCategory '_')
  print ("DashPunctuation", generalCategory '-')
  print ("OpenPunctuation", generalCategory '(')
  print ("ClosePunctuation", generalCategory ')')
  print ("InitialQuote", generalCategory '\171')
  print ("FinalQuote", generalCategory '\187')
  print ("OtherPunctuation", generalCategory '!')
  print ("MathSymbol", generalCategory '+')
  print ("CurrencySymbol", generalCategory '$')
  print ("ModifierSymbol", generalCategory '^')
  print ("OtherSymbol", generalCategory '\169')
  print ("Space", generalCategory ' ')
  print ("LineSeparator", generalCategory '\8232')
  print ("ParagraphSeparator", generalCategory '\8233')
  print ("Control", generalCategory '\n')
  print ("Format", generalCategory '\173')
  print ("Surrogate", generalCategory '\55296')
  print ("PrivateUse", generalCategory '\57344')
  print ("NotAssigned", generalCategory '\65535')
  print ("isLetter agrees", all (\c -> isLetter c == (generalCategory c `elem` [UppercaseLetter, LowercaseLetter, TitlecaseLetter, ModifierLetter, OtherLetter])) ['\0' .. '\255'])
  print ("isPunctuation agrees", all (\c -> isPunctuation c == (generalCategory c >= ConnectorPunctuation && generalCategory c <= OtherPunctuation)) ['\0' .. '\255'])
  print ("isSymbol agrees", all (\c -> isSymbol c == (generalCategory c >= MathSymbol && generalCategory c <= OtherSymbol)) ['\0' .. '\255'])
  print ("isSeparator agrees", all (\c -> isSeparator c == (generalCategory c `elem` [Space, LineSeparator, ParagraphSeparator])) ['\0' .. '\255'])
