-- Haskell 2010 Report 2.6: a newline inside a string literal is only legal within a backslash-delimited gap
module Main (main) where

main :: IO ()
main = putStrLn "line one
                 line two"
