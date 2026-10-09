-- Haskell 2010 Report 4.3.3 / 11: Enum can be derived only for enumerations (all constructors nullary)
module Main (main) where

data T = A | B Int deriving (Enum)

main :: IO ()
main = putStrLn "unreachable"
