-- Haskell 2010 Report 5.2: with C(method) exported, the other methods of C are not in scope
module Main (main) where

import Lib

main :: IO ()
main = print (secretMethod (MkT 1))
