-- Haskell 2010 Report 6.3.1: function types are not instances of Eq
main :: IO ()
main = print (not == not)
