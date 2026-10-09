-- Haskell 2010 Report 4.3.2 / 5.2: an instance may only bind class methods that are in scope
module Main (main) where

import Lib

instance C Bool where
  method _ = 0
  secretMethod _ = 0

main :: IO ()
main = print (method True)
