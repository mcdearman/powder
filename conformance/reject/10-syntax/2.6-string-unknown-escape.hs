-- Haskell 2010 Report 2.6: only the listed escape codes are legal; \q is not one of them
module Main (main) where

main :: IO ()
main = putStrLn "bad \q escape"
