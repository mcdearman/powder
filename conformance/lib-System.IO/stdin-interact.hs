-- Haskell 2010 Report 41.8.2 (System.IO): interact passes all of standard input to a String -> String
-- function and writes the result to standard output
module Main (main) where

import Data.Char (toUpper)
import System.IO

number :: Int -> String -> String
number n l = show n ++ ": " ++ map toUpper l ++ " (" ++ show (length (words l)) ++ " words)"

main :: IO ()
main = do
  putStrLn "before interact"
  interact (unlines . zipWith number [1 ..] . lines)
  putStrLn "after interact"
