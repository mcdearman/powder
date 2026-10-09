-- Haskell 2010 Report 5.3.1, 20: an import list brings only the listed entities into scope; nub is not
-- in scope although Data.List is imported
module Main (main) where

import Data.List (sort)

main :: IO ()
main = print (nub (sort [3, 1, 3 :: Int]))
