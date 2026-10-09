-- Haskell 2010 Report 4.6: the unused parameter of Phantom defaults to kind * within its own dependency
-- group, so a later use at a type of kind * -> * (Phantom Maybe) is a kind error
module Main (main) where

data Phantom a = Phantom

value :: Phantom Maybe
value = Phantom

main :: IO ()
main = putStrLn "unreachable"
