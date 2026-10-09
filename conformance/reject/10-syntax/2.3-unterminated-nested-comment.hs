-- Haskell 2010 Report 2.3: nested comments must be properly closed; the inner comment's terminator does not close the outer one
module Main (main) where

{- outer {- inner -} never closed

main :: IO ()
main = putStrLn "unreachable"
