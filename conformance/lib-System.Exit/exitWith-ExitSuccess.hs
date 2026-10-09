-- Haskell 2010 Report 40 (System.Exit): exitWith ExitSuccess terminates the program with status 0;
-- output written before it appears, statements after it do not run
module Main (main) where

import System.Exit

main :: IO ()
main = do
  putStrLn "before exit"
  putStr "no trailing newline before exit"
  exitWith ExitSuccess
  putStrLn "unreachable"
