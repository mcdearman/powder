-- Haskell 2010 Report 9 (PreludeText): showsPrec on numbers; negative numbers are parenthesised when the precedence is above 6
-- (Rational uses precedence 7 for its `%`)
module Main (main) where

import Data.Ratio ((%))

precs :: [Int]
precs = [0 .. 11]

table :: String -> (Int -> ShowS) -> IO ()
table label f = mapM_ (\d -> putStrLn (label ++ " " ++ show d ++ ": " ++ f d "")) precs

main :: IO ()
main = do
  table "Int -5" (\d -> showsPrec d (-5 :: Int))
  table "Int 5" (\d -> showsPrec d (5 :: Int))
  table "Int 0" (\d -> showsPrec d (0 :: Int))
  table "Integer -5" (\d -> showsPrec d (-5 :: Integer))
  table "Integer big" (\d -> showsPrec d (negate (2 ^ (70 :: Int)) :: Integer))
  table "Double -1.5" (\d -> showsPrec d (-1.5 :: Double))
  table "Double 1.5" (\d -> showsPrec d (1.5 :: Double))
  table "Float -0.25" (\d -> showsPrec d (-0.25 :: Float))
  table "Rational 3 % 4" (\d -> showsPrec d (3 % 4 :: Rational))
  table "Rational -3 % 4" (\d -> showsPrec d (-3 % 4 :: Rational))
  table "Just -5" (\d -> showsPrec d (Just (-5) :: Maybe Int))
  table "Just 5" (\d -> showsPrec d (Just 5 :: Maybe Int))
  table "Left -1.5" (\d -> showsPrec d (Left (-1.5) :: Either Double Int))
  table "Just (3 % 4)" (\d -> showsPrec d (Just (3 % 4) :: Maybe Rational))
  table "list" (\d -> showsPrec d [-1, 2 :: Int])
  table "tuple" (\d -> showsPrec d (-1 :: Int, -2.5 :: Double))
  table "Nothing" (\d -> showsPrec d (Nothing :: Maybe Int))
  table "minBound" (\d -> showsPrec d (minBound :: Int))
  print ("continuation", showsPrec 0 (12 :: Int) "tail", showsPrec 11 (-12 :: Int) "tail", showsPrec 11 (Just 'x') "tail")
  print ("show = showsPrec 0", show (-7 :: Int) == showsPrec 0 (-7 :: Int) "", shows (-7 :: Int) "" == showsPrec 0 (-7 :: Int) "")
  print ("negative precedence", showsPrec (-1) (-7 :: Int) "", showsPrec 100 (-7 :: Int) "", showsPrec 100 (Just 'x') "")
