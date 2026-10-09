-- Haskell 2010 Report 4.1.1: kind error: the argument of a function type must have kind *, but Maybe :: * -> *
module Main (main) where

unwrap :: Maybe -> Int
unwrap _ = 0

main :: IO ()
main = putStrLn "unreachable"
