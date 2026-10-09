-- Haskell 2010 Report 9 (and 4.3.4): REJECT. In `show . read` the intermediate type is ambiguous (Show a, Read a) and
-- cannot be defaulted.
module Main (main) where

roundTrip :: String -> String
roundTrip = show . read

main :: IO ()
main = putStrLn (roundTrip "1")
