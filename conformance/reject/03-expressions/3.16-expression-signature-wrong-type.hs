-- Haskell 2010 Report 3.16: an expression's type signature must be a type the expression has; a string is
-- not an Int
main :: IO ()
main = print ("abc" :: Int)
