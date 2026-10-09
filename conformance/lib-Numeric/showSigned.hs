-- Haskell 2010 Report 38.1 (Numeric): showSigned (negative values parenthesised when precedence > 6)
module Main (main) where

import Data.Ratio
import Numeric

showFrac :: Rational -> ShowS
showFrac r = showInt (numerator r) . showChar '/' . showInt (denominator r)

main :: IO ()
main = do
  putStrLn (showSigned showInt 0 (5 :: Int) "")
  putStrLn (showSigned showInt 0 (-5 :: Int) "")
  putStrLn (showSigned showInt 6 (-5 :: Int) "")
  putStrLn (showSigned showInt 7 (-5 :: Int) "")
  putStrLn (showSigned showInt 11 (-5 :: Int) "")
  putStrLn (showSigned showInt 7 (5 :: Int) "")
  putStrLn (showSigned showInt 11 (5 :: Int) "")
  putStrLn (showSigned showInt 0 (0 :: Int) "")
  putStrLn (showSigned showInt 11 (0 :: Int) "")
  putStrLn (showSigned showInt 0 (-123456789012345678901234567890 :: Integer) "")
  putStrLn (showSigned showInt 9 (-123456789012345678901234567890 :: Integer) "")
  putStrLn (showSigned showInt 0 (-9223372036854775807 :: Int) "")
  putStrLn (showSigned showHex 0 (-255 :: Int) "")
  putStrLn (showSigned showHex 7 (-255 :: Int) "")
  putStrLn (showSigned showHex 7 (255 :: Int) "")
  putStrLn (showSigned showOct 0 (-8 :: Int) "")
  putStrLn (showSigned showFloat 0 (-1.5 :: Double) "")
  putStrLn (showSigned showFloat 7 (-1.5 :: Double) "")
  putStrLn (showSigned showFloat 7 (1.5 :: Double) "")
  putStrLn (showSigned showFrac 0 ((-3) % 4) "")
  putStrLn (showSigned showFrac 7 ((-3) % 4) "")
  putStrLn (showSigned showFrac 7 (3 % 4) "")
  putStrLn (showSigned showInt 0 (-5 :: Int) " trailing")
  putStrLn (showSigned showInt 7 (-5 :: Int) " trailing")
  putStrLn ((showString "x = " . showSigned showInt 0 (-1 :: Int) . showString ", y = " . showSigned showInt 7 (-2 :: Int)) "")
  print ("agrees with showsPrec", all (\(p, n) -> showSigned showInt p n "" == showsPrec p n "") [(p, n) | p <- [0, 6, 7, 10, 11], n <- [-12, -1, 0, 1, 12 :: Int]])
  print ("positive never parenthesised", all (\p -> showSigned showInt p (3 :: Int) "" == "3") [0 .. 11])
