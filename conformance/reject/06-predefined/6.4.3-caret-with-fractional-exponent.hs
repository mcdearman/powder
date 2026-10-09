-- Haskell 2010 Report 6.4.3: (^) requires an Integral exponent; a Double exponent needs (**)
main :: IO ()
main = print ((2 :: Double) ^ (0.5 :: Double))
