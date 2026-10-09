-- Haskell 2010 Report 39: getArgs :: IO [String] is an action, not a list of strings; its result must be
-- bound before it can be used as a list
module Main (main) where

import System.Environment

main :: IO ()
main = putStrLn (unwords getArgs)
