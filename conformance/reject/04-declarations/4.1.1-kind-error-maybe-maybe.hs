-- Haskell 2010 Report 4.1.1: kind error: Maybe :: * -> * is applied to Maybe, which does not have kind *
module Main (main) where

count :: Maybe Maybe -> Int
count _ = 0

main :: IO ()
main = putStrLn "unreachable"
