-- Haskell 2010 Report 40 (System.Exit): exitWith (ExitFailure 3) terminates the program with status 3
module Main (main) where

import System.Exit

main :: IO ()
main = do
  putStrLn "before exit"
  print (ExitFailure 3)
  exitWith (ExitFailure 3)
  putStrLn "unreachable"
