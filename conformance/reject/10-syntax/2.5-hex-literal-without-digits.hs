-- Haskell 2010 Report 2.5: 0x must be followed by a hexadecimal digit; "0xg" lexes as 0 applied to xg, which is unbound
module Main (main) where

main :: IO ()
main = print (0xg :: Int)
