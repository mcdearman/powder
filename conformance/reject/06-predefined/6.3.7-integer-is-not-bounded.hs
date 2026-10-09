-- Haskell 2010 Report 6.3.7: Integer is not an instance of Bounded
main :: IO ()
main = print (minBound :: Integer)
