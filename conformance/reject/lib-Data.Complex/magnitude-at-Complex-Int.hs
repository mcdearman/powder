-- Haskell 2010 Report 17: magnitude :: RealFloat a => Complex a -> a; Int is not an instance of RealFloat
module Main (main) where

import Data.Complex

n :: Int
n = 3

main :: IO ()
main = print (magnitude (n :+ 4))
