-- Haskell 2010 Report 11.2: REJECT. Enum can only be derived for enumerations (types whose constructors are all nullary);
-- here the constructor B has a field.
module Main (main) where

data T = A | B Int deriving (Enum)

main :: IO ()
main = return ()
