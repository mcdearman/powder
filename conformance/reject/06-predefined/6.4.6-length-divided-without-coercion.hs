-- Haskell 2010 Report 6.4.6: there are no implicit numeric coercions; an Int (length) cannot divide a Double without fromIntegral
average :: [Double] -> Double
average xs = sum xs / length xs

main :: IO ()
main = print (average [1.0, 2.0, 3.0])
