-- Haskell 2010 Report 3.3: what is applied must be a function; True takes no argument
main :: IO ()
main = print (True True)
