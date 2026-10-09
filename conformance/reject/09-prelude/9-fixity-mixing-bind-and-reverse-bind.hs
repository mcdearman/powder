-- Haskell 2010 Report 9 (and 4.4.2, 10.6): REJECT. The Prelude declares `infixl 1 >>=` and `infixr 1 =<<`: operators of
-- the same precedence but different associativity cannot be mixed without parentheses.
module Main (main) where

main :: IO ()
main = print (Just 1 >>= Just =<< Just (2 :: Int))
