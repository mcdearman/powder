-- Haskell 2010 Report 16 (Data.Char): GeneralCategory(..) constructors and its Eq, Ord, Show, Read, Enum,
-- Bounded and Ix instances
module Main (main) where

import Data.Char
import Data.Ix

allCategories :: [GeneralCategory]
allCategories =
  [ UppercaseLetter, LowercaseLetter, TitlecaseLetter, ModifierLetter, OtherLetter
  , NonSpacingMark, SpacingCombiningMark, EnclosingMark
  , DecimalNumber, LetterNumber, OtherNumber
  , ConnectorPunctuation, DashPunctuation, OpenPunctuation, ClosePunctuation
  , InitialQuote, FinalQuote, OtherPunctuation
  , MathSymbol, CurrencySymbol, ModifierSymbol, OtherSymbol
  , Space, LineSeparator, ParagraphSeparator
  , Control, Format, Surrogate, PrivateUse, NotAssigned
  ]

main :: IO ()
main = do
  mapM_ print allCategories
  print ("count", length allCategories)
  print ("declaration order is enum order", allCategories == [minBound .. maxBound])
  print ("minBound", (minBound :: GeneralCategory))
  print ("maxBound", (maxBound :: GeneralCategory))
  print ("fromEnum", map fromEnum allCategories)
  print ("toEnum", (toEnum 8 :: GeneralCategory), (toEnum 29 :: GeneralCategory))
  print ("succ pred", succ Space, pred Space)
  print ("enumFromTo", [DecimalNumber .. OtherNumber])
  print ("enumFromThenTo", [UppercaseLetter, OtherLetter .. Space])
  print ("enumFromThen", [Control, Surrogate ..])
  print ("eq", Space == Space, Space /= Control, Format == Control)
  print ("ord", UppercaseLetter < LowercaseLetter, compare Control Space, compare Space Space)
  print ("max min", max MathSymbol OtherSymbol, min MathSymbol OtherSymbol)
  print ("sorted already", and (zipWith (<) allCategories (tail allCategories)))
  print ("show", show OtherPunctuation)
  print ("showsPrec 11", showsPrec 11 MathSymbol "")
  print ("show in Just", Just CurrencySymbol)
  print ("read", (read "LineSeparator" :: GeneralCategory))
  print ("read with spaces and parens", (read " ( PrivateUse ) " :: GeneralCategory))
  print ("read . show", all (\c -> read (show c) == c) allCategories)
  print ("reads rest", (reads "Control!" :: [(GeneralCategory, String)]))
  print ("reads failure", (reads "Nonsense" :: [(GeneralCategory, String)]))
  print ("ix range", range (MathSymbol, OtherSymbol))
  print ("ix index", index (minBound, maxBound) Space)
  print ("ix inRange", inRange (DecimalNumber, OtherNumber) LetterNumber, inRange (DecimalNumber, OtherNumber) Space)
  print ("ix rangeSize", rangeSize (minBound :: GeneralCategory, maxBound))
