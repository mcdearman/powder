-- Haskell 2010 Report 40 (System.Exit): exitFailure is exitWith (ExitFailure exitfail), status 1
module Main (main) where

import System.Exit

check :: Int -> IO Int
check n
  | n < 3 = return (n * 2)
  | otherwise = putStrLn ("giving up at " ++ show n) >> exitFailure

main :: IO ()
main = do
  putStrLn "before exit"
  rs <- mapM (\n -> check n >>= \r -> print r >> return r) [0, 1, 2, 3, 4, 5]
  print ("unreachable", rs)
  putStrLn "unreachable"
