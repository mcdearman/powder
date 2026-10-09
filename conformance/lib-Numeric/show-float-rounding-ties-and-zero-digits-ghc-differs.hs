-- Haskell 2010 Report 38.1 (Numeric): showEFloat/showFFloat/showGFloat on exact decimal rounding ties and
-- with (Just 0) digits. GHC differs from the Report's reference code here: the Report's roundTo rounds a
-- final digit 5 up (0.25 -> "0.3") where GHC rounds half to even ("0.2"); the Report's showEFloat treats
-- (Just 0) as (Just 1) ("1.2e3") where GHC prints no fraction at all ("1e3").
module Main (main) where

import Numeric

main :: IO ()
main = do
  putStrLn (showFFloat (Just 1) (0.25 :: Double) "")
  putStrLn (showFFloat (Just 1) (0.75 :: Double) "")
  putStrLn (showFFloat (Just 2) (0.125 :: Double) "")
  putStrLn (showFFloat (Just 2) (0.375 :: Double) "")
  putStrLn (showFFloat (Just 1) (2.5e-2 :: Double) "")
  putStrLn (showEFloat (Just 1) (1.25 :: Double) "")
  putStrLn (showEFloat (Just 1) (1.75 :: Double) "")
  putStrLn (showEFloat (Just 3) (1234.5 :: Double) "")
  putStrLn (showGFloat (Just 1) (0.25 :: Double) "")
  putStrLn (showFFloat (Just 0) (2.7 :: Double) "")
  putStrLn (showFFloat (Just 0) (2.2 :: Double) "")
  putStrLn (showFFloat (Just 0) (0.5 :: Double) "")
  putStrLn (showFFloat (Just 0) (1.5 :: Double) "")
  putStrLn (showFFloat (Just 0) (2.5 :: Double) "")
  putStrLn (showFFloat (Just 0) (0 :: Double) "")
  putStrLn (showFFloat (Just (-1)) (3.7 :: Double) "")
  putStrLn (showEFloat (Just 0) (1234.5 :: Double) "")
  putStrLn (showEFloat (Just 0) (0 :: Double) "")
  putStrLn (showEFloat (Just 0) (9.6 :: Double) "")
  putStrLn (showEFloat (Just (-3)) (1234.5 :: Double) "")
  putStrLn (showGFloat (Just 0) (1234.5 :: Double) "")
  putStrLn (showGFloat (Just 0) (1.0e9 :: Double) "")
