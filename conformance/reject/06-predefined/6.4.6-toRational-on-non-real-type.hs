-- Haskell 2010 Report 6.4.6: toRational is a method of Real; Char is not an instance of Real
main :: IO ()
main = print (toRational 'a')
