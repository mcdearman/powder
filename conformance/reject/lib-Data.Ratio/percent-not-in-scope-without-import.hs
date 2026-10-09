-- Haskell 2010 Report 22, 5.3: (%) is exported by Data.Ratio, not by the Prelude (which exports only the
-- type Rational); using it without importing Data.Ratio is a scope error
module Main (main) where

half :: Rational
half = 1 % 2

main :: IO ()
main = print half
