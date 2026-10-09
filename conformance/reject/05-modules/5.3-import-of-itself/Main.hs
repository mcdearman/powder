-- Haskell 2010 Report 5.3: an import brings into scope what another module exports. A module
-- that imports itself asks for names that nothing defines.
module Main (main, x) where

import Main (x)

main :: IO ()
main = print (x :: Int)
