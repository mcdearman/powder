-- Haskell 2010 Report 21, 5.3: fromJust is exported by Data.Maybe, not by the Prelude; using it without
-- importing Data.Maybe is a scope error
module Main (main) where

main :: IO ()
main = print (fromJust (Just 'x'))
