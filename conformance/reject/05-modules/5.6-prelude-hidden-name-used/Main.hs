-- Haskell 2010 Report 5.6: an explicit import of Prelude replaces the implicit one, so hidden Prelude names are not in scope
module Main (main) where

import Prelude hiding (map)
import Lib

main :: IO ()
main = print (map (+ visible) [1, 2, 3 :: Int])
