-- Haskell 2010 Report 4.1.3: a class assertion in a context must have the form C a or C (a t1 ... tn);
-- `Eq [a]` applies the class to a type that is not headed by a type variable
module Main (main) where

same :: Eq [a] => a -> a -> Bool
same x y = [x] == [y]

main :: IO ()
main = putStrLn "unreachable"
