-- Haskell 2010 Report 5.2 / 5.8: a type exported without constructors is abstract; its constructor is not in scope
module Main (main) where

import Lib

unwrap :: Abstract -> Int
unwrap (MkAbstract n) = n

main :: IO ()
main = print (unwrap (mkAbstract 1))
