-- Haskell 2010 Report 11.4: constructor arguments are shown with showsPrec 11, so negative numbers are parenthesised
-- (`Just (-1)`, `P (-1) (-2.5)`); in record syntax fields are shown at precedence 0 (`f = -1`)
module Main (main) where

import Data.Ratio ((%))

data P = P Int Double deriving (Show)

data Q = Q Integer Float Rational deriving (Show)

data R = R {ri :: Int, rd :: Double} deriving (Show)

data N = N P (Maybe Int) [Int] (Int, Double) deriving (Show)

newtype W = W Int deriving (Show)

main :: IO ()
main = do
  print (Just (-1 :: Int))
  print (Just (1 :: Int))
  print (Just (-1.5 :: Double))
  print (Just (0 :: Int), Just (-0.0 :: Double))
  print (P 1 2.5)
  print (P (-1) 2.5)
  print (P 1 (-2.5))
  print (P (-1) (-2.5))
  print (P minBound (-1.0e-3))
  print (Q (-1) (-0.5) (-1 % 2))
  print (Q 1 0.5 (1 % 2))
  print (Q (negate (2 ^ (70 :: Int))) (-1.0e10) (3 % (-4)))
  print (R 1 2.5)
  print (R (-1) (-2.5))
  print (R {rd = -0.5, ri = -7})
  print (Just (R (-1) (-2.5)))
  print (N (P (-1) (-1)) (Just (-2)) [-3, 4, -5] (-6, -7.5))
  print (N (P 1 1) Nothing [] (0, 0))
  print (W (-3), [W (-3), W 3], Just (W (-3)))
  print (Left (-1) :: Either Int Int, Right (-1) :: Either Int Int)
  print [Just (-1), Nothing, Just (2 :: Int)]
  print (Just (Just (-1 :: Int)), Just [-1 :: Int], Just (-1 :: Int, -2 :: Int))
  putStrLn (showsPrec 11 (P (-1) (-2.5)) "")
  putStrLn (showsPrec 10 (P (-1) (-2.5)) "")
  putStrLn (showsPrec 11 (R (-1) (-2.5)) "")
  putStrLn (showsPrec 11 (W (-3)) "" ++ " " ++ showsPrec 10 (W (-3)) "" ++ " " ++ showsPrec 0 (W 3) "")
  putStrLn (showsPrec 6 (-1 :: Int) "" ++ " " ++ showsPrec 7 (-1 :: Int) "" ++ " " ++ showsPrec 6 (-1.5 :: Double) "" ++ " " ++ showsPrec 7 (-1.5 :: Double) "")
