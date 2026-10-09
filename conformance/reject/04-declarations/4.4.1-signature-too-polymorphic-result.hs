-- Haskell 2010 Report 4.4.1: the declared type claims any result type b, but the body returns its argument,
-- so the principal type a -> a is not as general as the signature a -> b
module Main (main) where

convert :: a -> b
convert x = x

main :: IO ()
main = putStrLn "unreachable"
