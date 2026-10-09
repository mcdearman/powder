-- Haskell 2010 Report 11.4: derived Show for types with Double, Float, Integer and Rational fields
-- (a Rational argument is an infix `%` expression of precedence 7, so it is parenthesised as a constructor argument)
module Main (main) where

import Data.Ratio ((%))

data R = R Rational deriving (Show)

data D = D Double Float deriving (Show)

data N = N Integer Rational Double deriving (Show)

data Rec = Rec {ratio :: Rational, dbl :: Double} deriving (Show)

infixl 7 :/

data Q = Rational :/ Rational deriving (Show)

infixl 6 :+

data S = Rational :+ Double deriving (Show)

main :: IO ()
main = do
  print (R (1 % 2))
  print (R (-1 % 2))
  print (R 3)
  print (R (6 % 8))
  print (R 0)
  print (Just (R (1 % 2)), [R (1 % 3), R (2 % 3)])
  print (D 1.5 2.5)
  print (D (-1.5) (-2.5))
  print (D 1.0e-2 1.0e10)
  print (D 0.1 0.1)
  print (D (1 / 0) (-1 / 0))
  print (D 5.0e-324 3.4028235e38)
  print (D (-0.0) 0)
  print (N 12345678901234567890 (1 % 3) 0.5)
  print (N (-12345678901234567890) (-1 % 3) (-0.5))
  print (N (2 ^ (64 :: Int)) (2 ^ (64 :: Int) % 3) 1.0e100)
  print (Rec (1 % 2) 1.5)
  print (Rec (-1 % 2) (-1.5))
  print (Rec {dbl = 0.1 + 0.2, ratio = 3})
  print (Just (Rec (5 % 7) 2))
  print ((1 % 2) :/ (3 % 4))
  print ((-1 % 2) :/ 3)
  print (Just ((1 % 2) :/ (3 % 4)))
  print ((1 % 2) :+ 1.5)
  print ((-1 % 2) :+ (-1.5))
  print (Just (1 % 2 :: Rational), Just (-1 % 2 :: Rational), [1 % 2, -3 % 4 :: Rational])
  print (1 % 2 :: Rational, (1 % 2 :: Rational, 0.5 :: Double))
  putStrLn (showsPrec 7 (1 % 2 :: Rational) "" ++ " | " ++ showsPrec 8 (1 % 2 :: Rational) "" ++ " | " ++ showsPrec 11 (R (1 % 2)) "")
