-- Haskell 2010 Report 6.1.2 / 6.4: Char is not an instance of Num; characters are converted with fromEnum/toEnum, not arithmetic
main :: IO ()
main = print ('a' + 1)
