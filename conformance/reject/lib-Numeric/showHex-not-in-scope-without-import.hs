-- Haskell 2010 Report 38, 5.3: showHex is exported by Numeric, not by the Prelude; using it without
-- importing Numeric is a scope error
module Main (main) where

main :: IO ()
main = putStrLn (showHex (255 :: Int) "")
