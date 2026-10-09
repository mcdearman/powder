-- Haskell 2010 Report 41.7.2: hPutStr :: Handle -> String -> IO (); a FilePath (String) is not a Handle
module Main (main) where

import System.IO

path :: FilePath
path = "/tmp/powder-conformance-hPutStr-given-FilePath"

main :: IO ()
main = hPutStr path "text"
