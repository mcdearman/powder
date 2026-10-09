-- Haskell 2010 Report 15: shiftL :: Bits a => a -> Int -> a; the shift amount must be an Int, not an
-- Integer
module Main (main) where

import Data.Bits

amount :: Integer
amount = 3

main :: IO ()
main = print (shiftL (1 :: Int) amount)
