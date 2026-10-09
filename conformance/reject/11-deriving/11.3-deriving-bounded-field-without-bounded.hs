-- Haskell 2010 Report 11 (condition 3) and 11.3: REJECT. Deriving Bounded for a single-constructor type needs a Bounded
-- instance for every field type, and Integer is not Bounded.
module Main (main) where

data T = T Bool Integer deriving (Bounded)

main :: IO ()
main = return ()
