-- Haskell 2010 Report 17 (Data.Complex): Num instance (+, -, *, negate, abs, signum, fromInteger), exact cases
module Main (main) where

import Data.Complex

type C = Complex Double

i :: C
i = 0 :+ 1

main :: IO ()
main = do
  print ("add", (1 :+ 2) + (3 :+ 4) :: C)
  print ("subtract", (1 :+ 2) - (3 :+ 5) :: C)
  print ("subtract self", (1 :+ 2) - (1 :+ 2) :: C)
  print ("multiply", (1 :+ 2) * (3 :+ 4) :: C)
  print ("multiply commutes", (3 :+ 4) * (1 :+ 2) :: C)
  print ("multiply reals", (2 :+ 0) * (3 :+ 0) :: C)
  print ("i squared", i * i)
  print ("i cubed", i * i * i == negate i)
  print ("i to the fourth", i * i * i * i == 1)
  print ("power", (1 :+ 1) ^ (2 :: Int) :: C)
  print ("power 3", (1 :+ 2) ^ (3 :: Int) :: C)
  print ("negate", negate (1 :+ 2) :: C)
  print ("negate mixed signs", negate ((-1.5) :+ 2.5) :: C)
  print ("unary minus", -(3 :+ (-4)) :: C)
  print ("abs 3 4", abs (3 :+ 4) :: C)
  print ("abs negative parts", abs ((-3) :+ (-4)) :: C)
  print ("abs 5 12", abs (5 :+ 12) :: C)
  print ("abs 8 15", abs (8 :+ 15) :: C)
  print ("abs zero", abs (0 :+ 0) :: C)
  print ("signum zero", signum (0 :+ 0) :: C)
  print ("signum 3 4", signum (3 :+ 4) :: C)
  print ("signum imaginary", signum (0 :+ 2) :: C)
  print ("signum negative real", signum ((-5) :+ 0) :: C)
  print ("signum negative imaginary", signum (0 :+ (-2)) :: C)
  print ("abs times signum", abs (3 :+ 4) * signum (3 :+ 4) == (3 :+ 4 :: C))
  print ("fromInteger", (fromInteger 7 :: C))
  print ("fromIntegral", (fromIntegral (3 :: Int) :: C))
  print ("literal arithmetic", 2 * (1 :+ 1) + 1 :: C)
  print ("sum", sum [1 :+ 1, 2 :+ 2, 3 :+ 3 :: C])
  print ("product", product [1 :+ 1, 1 :+ (-1) :: C])
  print ("distributes", (1 :+ 2) * ((3 :+ 4) + (5 :+ 6)) == ((1 :+ 2) * (3 :+ 4) + (1 :+ 2) * (5 :+ 6) :: C))
  print ("float", (1.5 :+ 0.5) * (2 :+ 2) :: Complex Float)
