-- Haskell 2010 Report 11 (condition 3): REJECT. Deriving Read needs a Read instance for every field type; U has none.
module Main (main) where

data U = U

data T = T Int U deriving (Read)

main :: IO ()
main = return ()
