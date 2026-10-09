-- Haskell 2010 Report 6.4.2 / Prelude: gcd 0 0.
-- GHC DIFFERS: the Report's Prelude defines `gcd 0 0 = error "Prelude.gcd: gcd 0 0 is undefined"`, so a Report-faithful
-- implementation terminates with an error here. GHC returns 0.
main :: IO ()
main = do
  print ("gcd 0 0 Int", gcd 0 0 :: Int)
  print ("gcd 0 0 Integer", gcd 0 0 :: Integer)
