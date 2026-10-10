-- Haskell 2010 Report 16 (Data.Char): GeneralCategory's constructors are matched in case alternatives
module Main (main) where

import Data.Char (GeneralCategory (..), generalCategory)

kind :: Char -> String
kind c = case generalCategory c of
  LowercaseLetter -> "lower"
  UppercaseLetter -> "upper"
  DecimalNumber -> "digit"
  MathSymbol -> "math"
  Space -> "space"
  Control -> "control"
  _ -> "other"

main :: IO ()
main = do
  print (generalCategory 'a')
  print (generalCategory '+')
  print (map kind "aZ5+ \n-")
  print (generalCategory 'a' == LowercaseLetter, generalCategory '+' /= MathSymbol)
  print (generalCategory '$', generalCategory '_', generalCategory '(', generalCategory ')')
  print ([minBound .. maxBound :: GeneralCategory] !! 4, fromEnum MathSymbol, succ Space)
