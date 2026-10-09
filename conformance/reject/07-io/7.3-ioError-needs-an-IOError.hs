-- Haskell 2010 Report 7.3: ioError :: IOError -> IO a takes an IOError (e.g. from userError), not a String
main :: IO ()
main = ioError "boom"
