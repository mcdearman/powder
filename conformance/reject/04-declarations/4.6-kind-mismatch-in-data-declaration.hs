-- Haskell 2010 Report 4.6: kind inference fails: the parameter f is used both as a type of kind * (field 1)
-- and as a type constructor of kind * -> * (field 2)
module Main (main) where

data Bad f a = Bad f (f a)

main :: IO ()
main = putStrLn "unreachable"
