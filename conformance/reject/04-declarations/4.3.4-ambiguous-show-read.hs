-- Haskell 2010 Report 4.3.4: `show (read s)` has the ambiguous type (Show a, Read a) => String; no numeric
-- class is involved, so defaulting does not apply
module Main (main) where

main :: IO ()
main = putStrLn (show (read "1"))
