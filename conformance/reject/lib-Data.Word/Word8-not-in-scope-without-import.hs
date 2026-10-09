-- Haskell 2010 Report 23, 5.3: the type Word8 is exported by Data.Word, not by the Prelude; using it
-- without importing Data.Word is a scope error
module Main (main) where

byte :: Word8
byte = 5

main :: IO ()
main = print byte
