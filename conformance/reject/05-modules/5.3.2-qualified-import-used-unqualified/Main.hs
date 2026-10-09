-- Haskell 2010 Report 5.3.2: import qualified brings only qualified names into scope
module Main (main) where

import qualified Lib

main :: IO ()
main = print visible
