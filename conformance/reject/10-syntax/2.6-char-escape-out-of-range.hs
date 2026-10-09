-- Haskell 2010 Report 2.6: numeric escapes must denote a Unicode code point (at most 1114111)
module Main (main) where

main :: IO ()
main = print (fromEnum '\1114112')
