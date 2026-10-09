-- Haskell 2010 Report 6.4 / 6.1.1: Bool is not an instance of Num, so an integer literal cannot be added to True
main :: IO ()
main = print (1 + True)
