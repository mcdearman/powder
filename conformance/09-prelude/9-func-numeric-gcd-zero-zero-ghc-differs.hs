-- Haskell 2010 Report 9: the Prelude code has `gcd 0 0 = error "Prelude.gcd: gcd 0 0 is undefined"`,
-- although 6.4.2 says gcd 0 0 = 0; GHC returns 0.  (GHC-DIFFERS from the Report's Prelude source.)
module Main (main) where

main :: IO ()
main = do
  print ("gcd 0 0 Int", gcd 0 0 :: Int)
  print ("gcd 0 0 Integer", gcd 0 0 :: Integer)
  print ("foldr gcd 0 []", foldr gcd 0 ([] :: [Int]))
