-- Haskell 2010 Report 4.2.2: a type synonym must be applied to all of its arguments; `Pair` is used unapplied
module Main (main) where

type Pair a = (a, a)

data W f = W (f Int)

value :: W Pair
value = W (1, 2)

main :: IO ()
main = putStrLn "unreachable"
