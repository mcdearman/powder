-- Haskell 2010 Report 4.3.2 / 5.4: a type may not be an instance of a class more than once in the program
module Main (main) where

import Lib
import Other ()

instance C Int where
  method = id

main :: IO ()
main = print (method (3 :: Int))
