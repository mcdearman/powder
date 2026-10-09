-- Haskell 2010 Report 5.3 / 5.5.1: a qualified name M.x requires an import declaration for M (here Data.Char is never imported)
module Main (main) where

import Lib

main :: IO ()
main = print (visible, Data.Char.toUpper 'a')
