-- Haskell 2010 Report 6.4.2: (/) is a method of Fractional; Int is not an instance of Fractional
main :: IO ()
main = print ((7 :: Int) / 2)
