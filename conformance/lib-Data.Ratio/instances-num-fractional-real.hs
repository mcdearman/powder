-- Haskell 2010 Report 22 (Data.Ratio): Num, Fractional and Real instances of Ratio (exact arithmetic)
module Main (main) where

import Data.Ratio

a :: Rational
a = 1 % 2

b :: Rational
b = (-3) % 4

harmonic :: Integer -> Rational
harmonic n = sum [1 % k | k <- [1 .. n]]

main :: IO ()
main = do
  print ("add", a + b)
  print ("add to integer", a + a)
  print ("add reduces", 1 % 6 + 1 % 3 :: Rational)
  print ("subtract", a - b)
  print ("subtract self", a - a)
  print ("multiply", a * b)
  print ("multiply reduces", (2 % 3) * (3 % 2) :: Rational)
  print ("negate", negate a, negate b, negate (0 :: Rational))
  print ("abs", abs a, abs b, abs (0 :: Rational))
  print ("signum", signum a, signum b, signum (0 :: Rational))
  print ("abs times signum", abs b * signum b == b)
  print ("fromInteger", (fromInteger 7 :: Rational), (fromInteger (-7) :: Rational))
  print ("fromIntegral", (fromIntegral (3 :: Int) :: Rational))
  print ("power", a ^ (10 :: Int), b ^ (3 :: Int), b ^ (0 :: Int))
  print ("sum", sum [1 % 2, 1 % 3, 1 % 6 :: Rational])
  print ("product", product [1 % 2, 2 % 3, 3 % 4 :: Rational])
  print ("harmonic 10", harmonic 10)
  print ("harmonic 20", harmonic 20)
  print ("exactness", (1 % 10 + 2 % 10 :: Rational) == 3 % 10, (0.1 + 0.2 :: Double) == 0.3)
  print ("divide", a / b)
  print ("divide by integer", a / 2)
  print ("divide to integer", (3 % 4) / (1 % 4) :: Rational)
  print ("divide negatives", b / b, 1 / b)
  print ("recip", recip a, recip b, recip (5 :: Rational))
  print ("recip keeps sign in numerator", numerator (recip b), denominator (recip b))
  print ("negative integer power", a ^^ (-3 :: Int), b ^^ (-2 :: Int), b ^^ (-1 :: Int))
  print ("fromRational", (fromRational (3 % 4) :: Rational))
  print ("fromRational to Double", (fromRational (3 % 4) :: Double), (fromRational ((-1) % 8) :: Double), (fromRational (1 % 3) :: Double))
  print ("fromRational to Float", (fromRational (3 % 4) :: Float), (fromRational (1 % 3) :: Float))
  print ("toRational of Rational", toRational b)
  print ("toRational of Ratio Int", toRational (3 % 6 :: Ratio Int))
  print ("toRational of Double", toRational (0.75 :: Double), toRational (-2.5 :: Double), toRational (0 :: Double))
  print ("toRational of 0.1 is not one tenth", toRational (0.1 :: Double) == 1 % 10, toRational (0.1 :: Double))
  print ("toRational of Float", toRational (0.1 :: Float))
  print ("toRational of integers", toRational (5 :: Int), toRational (-5 :: Integer))
  print ("realToFrac", (realToFrac b :: Double), (realToFrac (0.5 :: Double) :: Rational))
  print ("Ratio Int arithmetic", (1 % 2 + 1 % 3 :: Ratio Int), ((1 % 2) * (2 % 3) :: Ratio Int), ((1 % 2) / 3 :: Ratio Int))
  print ("mean", sum [1, 2, 4 :: Rational] / 3)
