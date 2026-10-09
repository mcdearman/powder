-- Haskell 2010 Report 40 (System.Exit): exitWith with a computed ExitFailure code (42), called from a
-- nested action; pending stdout output is still delivered
module Main (main) where

import System.Exit

finish :: [Int] -> IO a
finish xs = do
  putStrLn ("sum is " ++ show (sum xs))
  exitWith (if sum xs == 0 then ExitSuccess else ExitFailure (sum xs))

main :: IO ()
main = do
  putStrLn "before exit"
  mapM_ print [1, 2, 3 :: Int]
  _ <- finish [10, 12, 20]
  putStrLn "unreachable"
