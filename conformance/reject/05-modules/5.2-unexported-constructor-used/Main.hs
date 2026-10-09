-- Haskell 2010 Report 5.2: with T(MkT) exported, the other constructors of T are not in scope
module Main (main) where

import Lib

main :: IO ()
main = print (method (Private 1))
