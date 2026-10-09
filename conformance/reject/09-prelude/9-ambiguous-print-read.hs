-- Haskell 2010 Report 9 (and 4.3.4): REJECT. In `print (read "1")` the type of the value read is ambiguous: the
-- constraints (Read a, Show a) mention no numeric class, so defaulting does not apply.
module Main (main) where

main :: IO ()
main = print (read "1")
