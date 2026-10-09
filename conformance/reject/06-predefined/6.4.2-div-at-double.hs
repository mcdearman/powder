-- Haskell 2010 Report 6.4.2: div is a method of Integral; Double is not an instance of Integral
main :: IO ()
main = print ((7.0 :: Double) `div` 2.0)
