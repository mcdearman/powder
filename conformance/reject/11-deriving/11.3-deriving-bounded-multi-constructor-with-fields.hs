-- Haskell 2010 Report 11.3: REJECT. Bounded can only be derived for enumerations and for types with a single
-- constructor; this type has two constructors and one of them has a field.
module Main (main) where

data T = A Bool | B deriving (Bounded)

main :: IO ()
main = return ()
