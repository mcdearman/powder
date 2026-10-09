-- Haskell 2010 Report 4.4.1 / 4.5.4: without a type signature a function is monomorphic in its own body,
-- so the recursive call of `depth` at type Nested [a] cannot be typed
module Main (main) where

data Nested a = Flat a | Nest (Nested [a])

depth (Flat _) = 0
depth (Nest n) = 1 + depth n

main :: IO ()
main = putStrLn "unreachable"
