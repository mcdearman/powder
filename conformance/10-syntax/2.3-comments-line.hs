-- Haskell 2010 Report 2.3: line comments; "--" only starts a comment when not part of a longer operator symbol
module Main (main) where

infixl 6 -->
infixl 6 |--
infixl 6 --|

(-->) :: Int -> Int -> Int
a --> b = a * 10 + b

(|--) :: Int -> Int -> Int
a |-- b = a - b

(--|) :: Int -> Int -> Int
a --| b = a + 100 * b

main :: IO ()
main = do
  print ("arrow op", 1 --> 2)
  print ("bar-dash op", 9 |-- 4)
  print ("dash-dash-bar op", 1 --| 2)
  print ("plain", 5) -- an ordinary comment
  print ("three dashes", 6) --- still a comment
  print ("many dashes", 7) ---------- still a comment
  print ("dash then letter", 8) --this is a comment too
  print ("dash then quote", 9) --"not a string"
  print ("string with dashes", "a -- b")
  print ("string with braces", "{- not a comment -}")
  -- print ("commented out", 0)
  print ("minus minus applied", 3 - (-2))
  print ("end", ())
--
