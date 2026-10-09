-- Haskell 2010 Report 17 (Data.Complex): Fractional instance ((/), recip, fromRational), exact cases only
module Main (main) where

import Data.Complex
import Data.Ratio

type C = Complex Double

main :: IO ()
main = do
  print ("divide by real", (4 :+ 2) / (2 :+ 0) :: C)
  print ("divide reals", (10 :+ 0) / (2 :+ 0) :: C)
  print ("divide undoes multiply", ((-5) :+ 10) / (3 :+ 4) :: C)
  print ("divide by itself", (1 :+ 1) / (1 :+ 1) :: C)
  print ("divide by i", (1 :+ 0) / (0 :+ 1) :: C)
  print ("divide by imaginary", (3 :+ 4) / (0 :+ 2) :: C)
  print ("divide by literal", (6 :+ 8) / 2 :: C)
  print ("recip real", recip (2 :+ 0) :: C)
  print ("recip i", recip (0 :+ 1) :: C)
  print ("recip of power of two", recip (0 :+ 4) :: C)
  print ("fromRational", (fromRational (3 % 4) :: C))
  print ("fromRational negative", (fromRational ((-5) % 2) :: C))
  print ("fractional literal", (0.25 :: C))
  print ("realToFrac", (realToFrac (1.5 :: Double) :: C))
  print ("negative power", (0 :+ 2) ^^ (-1 :: Int) :: C)
  print ("zeroth power", (3 :+ 4) ^^ (0 :: Int) :: C)
  print ("positive power", (0 :+ 1) ^^ (2 :: Int) :: C)
  print ("multiply then divide", (((1 :+ 2) * (3 :+ 4)) / (3 :+ 4) :: C) == 1 :+ 2)
  print ("float divide", (4 :+ 2) / (2 :+ 0) :: Complex Float)
