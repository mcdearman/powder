-- Haskell 2010 Report 5.3.1: entities named in a hiding list are not imported
module Main (main) where

import Lib hiding (visible)

main :: IO ()
main = print visible
