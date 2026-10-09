-- Haskell 2010 Report 40 (System.Exit): exitWith (ExitFailure 255), the largest exit status that survives
-- on POSIX systems
module Main (main) where

import System.Exit
import System.IO

main :: IO ()
main = do
  putStrLn "before exit"
  hPutStrLn stderr "diagnostic on stderr (not compared)"
  hFlush stdout
  exitWith (ExitFailure 255)
  putStrLn "unreachable"
