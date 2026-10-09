-- Haskell 2010 Report 41.3.1: openFile :: FilePath -> IOMode -> IO Handle; applying it to the path alone
-- yields a function, not an IO action producing a Handle
module Main (main) where

import System.IO

main :: IO ()
main = do
  h <- openFile "/tmp/powder-conformance-openFile-missing-IOMode"
  hClose h
