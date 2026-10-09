-- Haskell 2010 Report 40 (System.Exit): exitSuccess is exitWith ExitSuccess; its type IO a lets it be
-- used where any result type is expected
module Main (main) where

import Control.Monad (when)
import System.Exit

countdown :: Int -> IO String
countdown n = do
  putStrLn ("count " ++ show n)
  when (n == 0) (putStrLn "lift off" >> exitSuccess)
  countdown (n - 1)

main :: IO ()
main = do
  putStrLn "before exit"
  s <- countdown 3
  putStrLn ("unreachable " ++ s)
