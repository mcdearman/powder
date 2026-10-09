-- Haskell 2010 Report 38.1, 38.3 (Numeric): floatToDigits (non-negative arguments) and fromRat
module Main (main) where

import Data.Ratio
import Numeric

value :: Integer -> ([Int], Int) -> Rational
value base (ds, e) = foldl (\acc d -> acc * fromInteger base + fromIntegral d) 0 ds * fromInteger base ^^ (e - length ds)

main :: IO ()
main = do
  print ("zero", floatToDigits 10 (0 :: Double))
  print ("one", floatToDigits 10 (1 :: Double))
  print ("one and a half", floatToDigits 10 (1.5 :: Double))
  print ("123.456", floatToDigits 10 (123.456 :: Double))
  print ("tenth", floatToDigits 10 (0.1 :: Double))
  print ("thousandth", floatToDigits 10 (0.001 :: Double))
  print ("0.015625", floatToDigits 10 (0.015625 :: Double))
  print ("large", floatToDigits 10 (1.0e100 :: Double))
  print ("small", floatToDigits 10 (1.0e-100 :: Double))
  print ("0.1 + 0.2", floatToDigits 10 (0.1 + 0.2 :: Double))
  print ("third", floatToDigits 10 (1 / 3 :: Double))
  print ("max double", floatToDigits 10 (1.7976931348623157e308 :: Double))
  print ("min denormal", floatToDigits 10 (5.0e-324 :: Double))
  print ("base 2 five", floatToDigits 2 (5 :: Double))
  print ("base 2 three quarters", floatToDigits 2 (0.75 :: Double))
  print ("base 2 half", floatToDigits 2 (0.5 :: Double))
  print ("base 2 one", floatToDigits 2 (1 :: Double))
  print ("base 2 zero", floatToDigits 2 (0 :: Double))
  print ("base 16", floatToDigits 16 (255 :: Double), floatToDigits 16 (0.5 :: Double), floatToDigits 16 (4096 :: Double))
  print ("base 8", floatToDigits 8 (64 :: Double), floatToDigits 8 (0.125 :: Double))
  print ("float tenth", floatToDigits 10 (0.1 :: Float))
  print ("float third", floatToDigits 10 (1 / 3 :: Float))
  print ("float 16777216", floatToDigits 10 (16777216 :: Float))
  print ("float base 2", floatToDigits 2 (0.15625 :: Float))
  print ("digits denote the value exactly in base 2", all (\x -> value 2 (floatToDigits 2 x) == toRational x) [0.5, 0.75, 5, 0.1, 123.456, 1.0e10 :: Double])
  print ("decimal digits read back", all (\x -> fromRat (value 10 (floatToDigits 10 x)) == x) [0.5, 0.1, 123.456, 1 / 3, 1.0e100, 1.0e-100 :: Double])
  print ("first digit is non-zero", all (\x -> head (fst (floatToDigits 10 x)) /= 0) [0.5, 0.001, 123.456, 1.0e-100 :: Double])
  print ("fromRat half", (fromRat (1 % 2) :: Double))
  print ("fromRat third", (fromRat (1 % 3) :: Double))
  print ("fromRat tenth", (fromRat (1 % 10) :: Double), (fromRat (1 % 10) :: Double) == 0.1)
  print ("fromRat negative", (fromRat ((-5) % 2) :: Double))
  print ("fromRat zero", (fromRat 0 :: Double))
  print ("fromRat integer", (fromRat 12345 :: Double))
  print ("fromRat large", (fromRat (10 ^ (30 :: Int)) :: Double))
  print ("fromRat small", (fromRat (1 % 10 ^ (30 :: Int)) :: Double))
  print ("fromRat float", (fromRat (3 % 4) :: Float), (fromRat (1 % 3) :: Float))
  print ("fromRat . toRational", all (\x -> fromRat (toRational x) == x) [0.1, -2.5, 1.0e-300, 1.0e300, 123.456 :: Double])
  print ("fromRat agrees with fromRational", all (\r -> (fromRat r :: Double) == fromRational r) [1 % 3, 22 % 7, (-1) % 1000, 123456789 % 1000])
