-- Haskell 2010 Report 40: exitWith :: ExitCode -> IO a; an Int is not an ExitCode
module Main (main) where

import System.Exit

code :: Int
code = 3

main :: IO ()
main = exitWith code
