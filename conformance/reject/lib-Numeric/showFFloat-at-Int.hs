-- Haskell 2010 Report 38.1: showFFloat :: RealFloat a => Maybe Int -> a -> ShowS; Int is not an instance
-- of RealFloat
module Main (main) where

import Numeric

n :: Int
n = 3

main :: IO ()
main = putStrLn (showFFloat (Just 2) n "")
