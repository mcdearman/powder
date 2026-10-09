-- Haskell 2010 Report 5.3.1, 21: it is an error to import an entity that the module does not export;
-- Data.Maybe exports no noSuchThing
module Main (main) where

import Data.Maybe (noSuchThing)

main :: IO ()
main = putStrLn "unreachable"
