-- Haskell 2010 Report 4.5.5 Rule 1: the restriction also applies to local simple pattern bindings:
-- `n` is monomorphic and cannot be both Int and Double
module Main (main) where

main :: IO ()
main = print (let n = 5 in (n + (1 :: Int), n + (0.5 :: Double)))
